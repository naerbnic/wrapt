package org.naerbnic.wrapt

import java.nio.ByteBuffer
import java.nio.channels.FileChannel

case class Header (stringTableOffset: Long, dataOffset: Long)

object Header {
  val MagicNumber = 0x0000000000000000L
  val Size = 24
  
  def fromFile(channel: FileChannel) = {
    val buffer = ByteBuffer.allocateDirect(Size)
    channel.read(buffer, 0)
    
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