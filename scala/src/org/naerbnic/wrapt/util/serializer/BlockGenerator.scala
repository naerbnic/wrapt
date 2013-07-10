package org.naerbnic.wrapt.util.serializer

import org.naerbnic.wrapt.util.Block
import java.nio.ByteBuffer
import BlockGenerator.MarkAtlas

trait BlockGenerator {
  def size: Long
  def createBlock(atlas: MarkAtlas): Block
}

object BlockGenerator {
  type MarkAtlas = Mark => Long
  
  def fromLongFunc(f: MarkAtlas => Long) = new BlockGenerator() {
    override def size = 8
    def createBlock(atlas: MarkAtlas) = {
      val buffer = ByteBuffer.allocate(8)
      buffer.asLongBuffer().put(0, f(atlas))
      Block.fromBuffer(buffer)
    }
  }
  
  def fromLong(v: Long) = fromLongFunc(_ => v)
  
  def fromIntFunc(f: MarkAtlas => Int) = new BlockGenerator() {
    override def size = 4
    def createBlock(atlas: MarkAtlas) = {
      val buffer = ByteBuffer.allocate(4)
      buffer.asLongBuffer().put(0, f(atlas))
      Block.fromBuffer(buffer)
    }
  }
  
  def fromInt(v: Int) = fromIntFunc(_ => v)
}