package org.naerbnic.wrapt.primitive

import org.naerbnic.wrapt.util.Block
import org.naerbnic.wrapt.BasicValue._
import java.nio.charset.Charset

sealed abstract class BlockIndexType(val code: Int) {
  def getValue(block: Block): PrimValue
}

object BlockIndexType {
  object BLK_STRING extends BlockIndexType(0x0) {
    def getValue(block: Block) = {
      val charbuf = Charset.forName("UTF-8").decode(block.asByteBuffer())
      PrimBasicValue(StringValue(charbuf.toString()))
    }
  }
  
  object BLK_MAP extends BlockIndexType(0x1) {
    def getValue(block: Block) = PrimMapValue(null)
  }
  
  object BLK_ARRAY extends BlockIndexType(0x2) {
    def getValue(block: Block) = PrimArrayValue(null)
  }
  
  object BLK_BLOB extends BlockIndexType(0x3) {
    def getValue(block: Block) = PrimBasicValue(BlobValue(block))
  }
  
  object BLK_INT extends BlockIndexType(0x4) {
    def getValue(block: Block) = PrimBasicValue(IntValue(block.readLong(0)))
  }
  
  object BLK_FLOAT extends BlockIndexType(0x5) {
    def getValue(block: Block) =
      PrimBasicValue(
          FloatValue(java.lang.Double.longBitsToDouble(block.readLong(0))))
  }
}