package org.naerbnic.wrapt.primitive

import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.nio.channels.FileChannel
import java.nio.channels.FileChannel.MapMode

class ByteBufferStringTable private (buffer: ByteBuffer) {
  val _buffer = buffer.duplicate()
  require (buffer.capacity() < Int.MaxValue);
  
  def getString(index: Int) = {
    _buffer.position(index)
    val length = ByteBufferStringTable.readPackedInt(_buffer)
    _buffer.limit(_buffer.position() + length)
    val string_data = _buffer.slice()
    
    val decoder = Charset.forName("UTF-8").newDecoder();
    decoder.decode(string_data).toString()
  }
}

object ByteBufferStringTable {
  private def readPackedInt(buffer: ByteBuffer): Int = {
    var result: Int = 0
    var b: Byte = 0
    do {
      b = buffer.get()
      result = result << 7 | 0x7f & b
    } while ((b & 0x80) != 0)
    
    return result;
  }
  
  def fromFile(channel: FileChannel, offset: Long) = {
    val sizeBuffer = ByteBuffer.allocateDirect(4);
    channel.read(sizeBuffer, offset)
    val size = sizeBuffer.asIntBuffer().get(0)
    require(size >= 0)
    new ByteBufferStringTable(channel.map(MapMode.READ_ONLY, offset + 4, size))
  }
}