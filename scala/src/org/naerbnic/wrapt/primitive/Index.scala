package org.naerbnic.wrapt

import java.nio.channels.FileChannel
import java.nio.ByteBuffer
import java.nio.channels.FileChannel.MapMode
import org.naerbnic.wrapt.primitive.MetaIndexEntry
import org.naerbnic.wrapt.primitive.IndexEntry

class Index private (metaIndexBuffer: ByteBuffer, indexBuffer: ByteBuffer) {
  lazy val numMetaIndexEntry = metaIndexBuffer.capacity() / 24
  
  private def metaIndexEntryBuffer(offset: Int) = {
    metaIndexBuffer.position(offset)
    metaIndexBuffer.limit(offset + MetaIndexEntry.Size)
    metaIndexBuffer.slice()
  }
  
  private def metaIndexEntry(offset: Int) =
    MetaIndexEntry.fromBuffer(metaIndexEntryBuffer(offset))
  
  private def indexEntryLong(offset: Int) =
    indexBuffer.getLong(offset * IndexEntry.Size)
    
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
  
  def indexEntry(virtualOffset: Int) = {
    for {
      physicalOffset <- findPhysicalOffsetFromVirtual(virtualOffset)
    } yield IndexEntry(indexEntryLong(physicalOffset))
  }
}

object Index {
  def fromFile(channel: FileChannel, offset: Long) = {
    val indexHeaderBuffer = ByteBuffer.allocate(8)
    channel.read(indexHeaderBuffer, offset)
    val indexHeaderIntBuffer = indexHeaderBuffer.asIntBuffer()
    
    val numMetaIndexEntries = indexHeaderIntBuffer.get(0)
    val numIndexEntries = indexHeaderIntBuffer.get(1)
    
    val metaIndexSize = numMetaIndexEntries * MetaIndexEntry.Size
    val metaIndexOffset = offset + indexHeaderBuffer.capacity()
    
    val metaIndexBuffer =
        channel.map(MapMode.READ_ONLY, metaIndexOffset, metaIndexSize)
        
    val indexSize = numIndexEntries * IndexEntry.Size
    val indexOffset = metaIndexOffset + metaIndexSize
    
    val indexBuffer =
        channel.map(MapMode.READ_ONLY, indexOffset, indexSize)
        
    new Index(metaIndexBuffer, indexBuffer)
  }
}