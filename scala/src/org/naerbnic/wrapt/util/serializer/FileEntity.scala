package org.naerbnic.wrapt.util.serializer

import org.naerbnic.wrapt.util.Block
import org.naerbnic.wrapt.util.serializer.FileComponent._

sealed trait FileEntity {
  def alignment: Int
  def size: Long
  protected[serializer] def marks(offset: Long): Map[Mark, Long]
  protected[serializer] def toBlock(markMap: Map[Mark, Long]): Block
  
  final def toBlock(): Block = toBlock(marks(0L))
}

object FileEntity {
  def ofBlock(block: Block): FileEntity =
    BlockEntity(BlockGenerator.fromBlock(block))
    
  def ofGen(gen: BlockGenerator): FileEntity = BlockEntity(gen)
  
  def concat(align: Int, components: Seq[FileComponent]): FileEntity = 
    CompositeEntity(align, components)
    
  private case class BlockEntity(block: BlockGenerator) extends FileEntity {
    override def alignment = 0
    override def size = block.size
    override protected[serializer] def marks(offset: Long) = Map()
    override protected[serializer] def toBlock(markMap: Map[Mark, Long]) = {
      block.createBlock(markMap)
    }
  }
  
  private case class CompositeEntity(
      alignment: Int,
      components: Seq[FileComponent])
      extends FileEntity {
    private case class ComponentRange(begin: Long, end: Long) {
      require (begin <= end)
      def size = end - begin
    }
    
    private def positions(start: Long) = {
      components.foldLeft(start -> Seq[ComponentRange]()) {
        (state, component) =>
          val (offset, ends) = state
          val newEnd = offset + component.size(offset)
          (newEnd, ends :+ ComponentRange(offset, newEnd))
      } . _2
    }
    
    lazy val size = {
      def offsetAlignment(offset: Long): Int = {
        for (i <- 1 until 64) {
          val bitmask = (1L << i) - 1
          if ((offset & bitmask) != 0) {
            return i;
          }
        }
        
        64;
      }
      
      for ((component, range) <- components zip positions(0L)) {
        val currAlignment =
          math.min(alignment, offsetAlignment(range.begin))
        require (currAlignment >= component.requiredAlignment)
      }
      
      positions(0L).lastOption.fold(0L)(_.end)
    }
    
    override protected[serializer] def marks(offset: Long) = {
      val markMapBuilder = Map.newBuilder[Mark, Long]
      
      for ((component, range) <- components zip positions(offset)) {
        component match {
          case MarkComponent(m) =>
            markMapBuilder += (m -> range.begin)
          case FileEntityComponent(ent) => 
            markMapBuilder ++= ent.marks(range.begin)
          case _ => {}
        }
      }
      
      markMapBuilder.result()
    }
    
    override protected[serializer] def toBlock(markMap: Map[Mark, Long]) = {
      val result = for {
        (component, range) <- components zip positions(0L)
      } yield component match {
        case AlignComponent(_) => Block.zeroes(range.size)
        case FileEntityComponent(ent) => ent.toBlock(markMap)
        case MarkComponent(_) => Block.NullBlock
      }
      
      Block.concat(result : _*)
    }
  }
  
}

