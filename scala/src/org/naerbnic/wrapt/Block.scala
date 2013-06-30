package org.naerbnic.wrapt

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.channels.FileChannel.MapMode

trait Block {
  def size: Long
  def asByteBuffer(): ByteBuffer
}

object Block {
  private class FileBlock(channel: FileChannel, fileOffset: Long, blockSize: Long) extends Block {
    override def size = blockSize
    override def asByteBuffer() =
      channel.map(MapMode.READ_ONLY, fileOffset, size)
  }
  
  def fromFile(channel: FileChannel, fileOffset: Long, blockSize: Long): Block =
    new FileBlock(channel, fileOffset, blockSize)
}