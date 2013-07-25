package org.naerbnic.wrapt.util.serializer

import org.naerbnic.wrapt.util.Block

sealed trait FileComponent {
  def requiredAlignment: Int
  def size(currOffset: Long): Long
}

object FileComponent {
  def ofBlock(block: Block): FileComponent =
    FileEntityComponent(FileEntity.ofBlock(block))
    
  def ofGen(gen: BlockGenerator): FileComponent =
    FileEntityComponent(FileEntity.ofGen(gen))
    
  def ofEntity(ent: FileEntity): FileComponent = FileEntityComponent(ent)
  
  def ofMark(mark: Mark): FileComponent = MarkComponent(mark)
  
  def ofAlign(align: Int): FileComponent = AlignComponent(align)
    
  private[serializer] case class MarkComponent(mark: Mark)
      extends FileComponent {
    def requiredAlignment = 0
    def size(currOffset: Long) = 0
  }
  
  private[serializer] case class AlignComponent(alignment: Int)
      extends FileComponent {
    def requiredAlignment = 0
    def size(currOffset: Long) = {
      val bitmask = (1L << alignment) - 1
      (-currOffset) & bitmask
    }
  }
  
  private[serializer] case class FileEntityComponent(fileEntity: FileEntity)
      extends FileComponent {
    def requiredAlignment = fileEntity.alignment
    def size(currOffset: Long) = fileEntity.size
  }
}