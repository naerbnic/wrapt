package org.naerbnic.wrapt.primitive

import java.nio.channels.FileChannel
import java.nio.ByteBuffer
import org.naerbnic.wrapt.Block

class PrimitiveWraptFile private (
    header: Header, block: Block) {
  val dataSegment = block.getSubBlock(header.dataOffset)
  lazy val stringTable =
    StringTable.fromFile(block, header.stringTableOffset)
  lazy val index = Index.fromFile(block, Header.Size)

  private def getBlockWithSize(dataOffset: Long, size: Int) = 
    dataSegment.getSubBlock(dataOffset, size)
  
  private def getBlockWithInlineSize(dataOffset: Long) = {
    val size = block.readLong(dataOffset)
    dataSegment.getSubBlock(dataOffset + 8, size)
  }
  
  private def getBlock(entry: BlockIndexEntry) = {
    entry.size.fold {
      getBlockWithInlineSize(entry.offset)
    } { size =>
      getBlockWithSize(entry.offset, size)
    } 
  }
  
  def getHandle(virtualIndex: Int) = 
    for {
      entry <- index.indexEntry(virtualIndex)
    } yield new PrimitiveWraptFile.Handle(entry, this)
}

object PrimitiveWraptFile {
  def fromFile(block: Block) =
    for { header <- Header.fromBlock(block) }
    yield new PrimitiveWraptFile(header, channel)
    
  class Handle(entry: IndexEntry, wraptFile: PrimitiveWraptFile) {
    def asInt = {
      if (entry.entryType == IndexEntry.Type.INT) {
        entry.literalInt.getOrElse {
          val buffer = wraptFile.getBlock(entry).asByteBuffer()
          require (buffer.capacity() == 8)
          Some(buffer.asLongBuffer().get(0))
        }
      } else {
        None
      }
    }
    
    def asFloat = {
      if (entry.entryType == IndexEntry.Type.FLOAT) {
        entry.literalFloat.getOrElse {
          val buffer = wraptFile.getBlock(entry).asByteBuffer()
          require (buffer.capacity() == 8)
          Some(buffer.asDoubleBuffer().get(0))
        }
      } else {
        None
      }
    }
    
    def asBool = {
      if (entry.entryType == IndexEntry.Type.BOOL) {
        Some(entry.literalBool.get)
      } else {
        None
      }
    }
    
    def isNull = entry.entryType == IndexEntry.Type.NULL
    
    def asArray = {
      if (entry.entryType == IndexEntry.Type.ARRAY) {
        Some(new PrimitiveArray(wraptFile.getBlock(entry)))
      } else {
        None
      }
    }
    
    def asMap = {
      if (entry.entryType == IndexEntry.Type.MAP) {
        Some(new PrimitiveMap(wraptFile.getBlock(entry), wraptFile.stringTable))
      } else {
        None
      }
    }
    
    def asBlob = {
      if (entry.entryType == IndexEntry.Type.BLOB) {
        Some(wraptFile.getBlock(entry))
      } else {
        None
      }
    }
  }
}