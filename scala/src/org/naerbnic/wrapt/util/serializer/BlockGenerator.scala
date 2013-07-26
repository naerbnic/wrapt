package org.naerbnic.wrapt.util.serializer

import org.naerbnic.wrapt.util.Block
import java.nio.ByteBuffer
import BlockGenerator.MarkAtlas
import org.naerbnic.wrapt.util.LongBits._

trait BlockGenerator {
  def size: Long
  def createBlock(atlas: MarkAtlas): Block
}

object BlockGenerator {
  type MarkAtlas = Mark => Long
  
  def fromLong(v: Long) = LongFunc.fromConst(v).generator
  
  def fromIntFunc(f: MarkAtlas => Int) = new BlockGenerator {
    override def size = 4
    def createBlock(atlas: MarkAtlas) = {
      val buffer = ByteBuffer.allocate(4)
      buffer.asLongBuffer().put(0, f(atlas))
      Block.fromBuffer(buffer)
    }
  }
  
  def fromInt(v: Int) = fromIntFunc(_ => v)
  
  def fromBlock(block: Block) = new BlockGenerator {
    override def size = block.size
    override def createBlock(dummy: MarkAtlas) = block
  }
}