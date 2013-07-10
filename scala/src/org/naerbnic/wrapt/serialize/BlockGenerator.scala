package org.naerbnic.wrapt.serialize

import org.naerbnic.wrapt.Block
import java.nio.ByteBuffer

trait BlockGenerator {
  def size: Long
  def createBlock(atlas: BlockGenerator.MarkAtlas): Block
}

object BlockGenerator {
  type MarkAtlas = Mark => Long
  
  def fromLong(f: MarkAtlas => Long) = new BlockGenerator() {
    override def size = 8
    def createBlock(atlas: MarkAtlas) = {
      val buffer = ByteBuffer.allocate(8)
      buffer.asLongBuffer().put(0, f(atlas))
      Block.fromBuffer(buffer)
    }
  }
  
  def fromInt(f: MarkAtlas => Int) = new BlockGenerator() {
    override def size = 8
    def createBlock(atlas: MarkAtlas) = {
      val buffer = ByteBuffer.allocate(4)
      buffer.asLongBuffer().put(0, f(atlas))
      Block.fromBuffer(buffer)
    }
  }
}