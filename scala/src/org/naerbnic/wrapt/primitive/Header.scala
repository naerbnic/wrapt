package org.naerbnic.wrapt.primitive

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import org.naerbnic.wrapt.Block

case class Header (stringTableOffset: Long, dataOffset: Long)

object Header {
  val MagicNumber = 0x0000000000000000L
  val Size = 24
  
  def fromBlock(block: Block) = {
    val buffer = ByteBuffer.allocateDirect(Size)
    block.read(buffer, 0)
    
    val magicNumber = buffer.getLong(0)
    val stringTableOffset = buffer.getLong(8)
    val dataOffset = buffer.getLong(16)
    
    if (magicNumber == MagicNumber) {
      Some(Header(stringTableOffset, dataOffset))
    } else {
      None
    }
  }
}