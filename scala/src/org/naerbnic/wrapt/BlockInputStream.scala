package org.naerbnic.wrapt

import java.io.InputStream
import java.nio.ByteBuffer

class BlockInputStream(block: Block) extends InputStream {
  var currPos = 0L
  
  override def read() = {
    val result = block.readByte(currPos)
    currPos += 1
    result
  }
  
  override def read(b: Array[Byte], off: Int, len: Int) = {
    val readLength : Int =
      if (currPos + len <= block.size) len else (block.size - currPos).toInt;
    val tempBuffer = ByteBuffer.wrap(b, off, readLength)
    block.read(tempBuffer, currPos)
    currPos += readLength
    readLength
  }
}