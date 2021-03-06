package org.naerbnic.wrapt.primitive.impl
import java.nio.ByteBuffer
import org.naerbnic.wrapt.util.Block
import org.naerbnic.wrapt.primitive.PrimValue
import scala.collection.immutable.SortedSet

class Index private (metaIndexBuffer: Block, indexBuffer: Block) {
  lazy val numMetaIndexEntry = metaIndexBuffer.size.toInt / 24
  
  private def metaIndexEntryBuffer(offset: Int) = {
    metaIndexBuffer.getSubBlock(offset, MetaIndexEntry.Size)
  }
  
  private def metaIndexEntry(offset: Int) =
    MetaIndexEntry.fromBuffer(metaIndexEntryBuffer(offset))
  
  private def indexEntryLong(offset: Int) =
    indexBuffer.readLong(offset * IndexEntry.Size)
    
  private def findPhysicalOffsetFromVirtual(virtualOffset: Int): Option[Int] = {
    def search(least: Int, most: Int): Option[Int] = {
      val median = (least + most) / 2
      require (least <= median)
      require (median < most)
      metaIndexEntry(median).search(virtualOffset) match {
        case MetaIndexEntry.Lower => search(least, median)
        case MetaIndexEntry.Higher => search(median + 1, most)
        case MetaIndexEntry.Found(physicalIndex) => Some(physicalIndex)
      }
    }
    
    search(0, numMetaIndexEntry)
  }
  
  def indexEntry(virtualOffset: Int): Option[IndexEntry] = {
    for {
      physicalOffset <- findPhysicalOffsetFromVirtual(virtualOffset)
      entry <- IndexEntry(indexEntryLong(physicalOffset))
    } yield entry
  }
  
  def entryIndexes = SortedSet[Int]()
}

object Index {
  def fromFile(file: Block, offset: Long) = {
    val indexHeaderBuffer = ByteBuffer.allocate(8)
    file.read(indexHeaderBuffer, offset)
    val indexHeaderIntBuffer = indexHeaderBuffer.asIntBuffer()
    
    val numMetaIndexEntries = indexHeaderIntBuffer.get(0)
    val numIndexEntries = indexHeaderIntBuffer.get(1)
    
    val metaIndexSize = numMetaIndexEntries * MetaIndexEntry.Size
    val metaIndexOffset = offset + indexHeaderBuffer.capacity()
    
    val metaIndexBuffer =
        file.getSubBlock(metaIndexOffset, metaIndexSize).reify()
        
    val indexSize = numIndexEntries * IndexEntry.Size
    val indexOffset = metaIndexOffset + metaIndexSize
    
    val indexBuffer = file.getSubBlock(indexOffset, indexSize)
        
    new Index(metaIndexBuffer, indexBuffer)
  }
}