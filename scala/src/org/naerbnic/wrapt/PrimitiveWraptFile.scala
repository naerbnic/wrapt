package org.naerbnic.wrapt

import java.nio.channels.FileChannel
import java.nio.ByteBuffer
import java.nio.LongBuffer
import java.nio.channels.FileChannel.MapMode

class PrimitiveWraptFile private (
    header: Header, channel: FileChannel) {
  lazy val stringTable =
    ByteBufferStringTable.fromFile(channel, header.stringTableOffset)
  lazy val index = Index.fromFile(channel, Header.Size)
  
  private def getBlockWithSize(dataOffset: Long, size: Int) = 
    Block.fromFile(channel, dataOffset + header.dataOffset, size)
  
  private def getBlockWithInlineSize(dataOffset: Long) = {
    val sizeBuffer = ByteBuffer.allocate(8)
    channel.read(sizeBuffer, dataOffset + header.dataOffset)
    val size = sizeBuffer.asLongBuffer().get(0)
    Block.fromFile(channel, dataOffset + header.dataOffset + 8, size)
  }
  
  private def getBlock(entry: IndexEntry) = {
    val size = entry.blockSize.get
    if (size == 0) {
      getBlockWithInlineSize(entry.offset.get)
    } else {
      getBlockWithSize(entry.offset.get, size)
    }
  }
  
  def getHandle(virtualIndex: Int) = 
    for {
      entry <- index.indexEntry(virtualIndex)
    } yield new PrimitiveWraptFile.Handle(entry, this)
}

object PrimitiveWraptFile {
  def fromFile(channel: FileChannel) =
    for { header <- Header.fromFile(channel) }
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