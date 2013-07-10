package org.naerbnic.wrapt.primitive.impl
import java.nio.charset.Charset
import org.naerbnic.wrapt.util.Block

class StringTable private (block: Block) {
  def getString(index: Int) = {
    require (index >= 0)
    val stringData = StringTable.getStringBlock(block, index)
    
    val decoder = Charset.forName("UTF-8").newDecoder();
    decoder.decode(stringData.asByteBuffer()).toString()
  }
}

object StringTable {
  private def getStringBlock(block: Block, index: Int) = {
    var currIndex = index
    var size: Int = 0
    var b: Byte = 0
    do {
      b = block.readByte(currIndex)
      currIndex += 1
      size = size << 7 | 0x7f & b
    } while ((b & 0x80) != 0)
    
    block.getSubBlock(currIndex, size)
  }
  
  def fromFile(block: Block, offset: Long) = {
    val size = block.readInt(offset)
    require(size >= 0)
    new StringTable(block.getSubBlock(offset + 4, size).reify())
  }
}