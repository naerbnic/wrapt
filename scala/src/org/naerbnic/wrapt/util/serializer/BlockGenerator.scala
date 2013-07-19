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
  
  private class LongBlockGenerator(f: MarkAtlas => Long) extends BlockGenerator {
    override def size = java.lang.Long.SIZE / java.lang.Byte.SIZE
    def createBlock(atlas: MarkAtlas) = {
      val buffer = ByteBuffer.allocate(size.toInt)
      buffer.asLongBuffer().put(0, f(atlas))
      Block.fromBuffer(buffer)
    }
  }
  
  private class IntBlockGenerator(f: MarkAtlas => Int) extends BlockGenerator {
    override def size = java.lang.Integer.SIZE / java.lang.Byte.SIZE
    def createBlock(atlas: MarkAtlas) = {
      val buffer = ByteBuffer.allocate(size.toInt)
      buffer.asIntBuffer().put(0, f(atlas))
      Block.fromBuffer(buffer)
    }
  }
  
  class IntFunc(val v: MarkAtlas => Int) extends AnyVal {
    private def merge(other: IntFunc)(f: (Int, Int) => Int) =
      new IntFunc((atlas) => f(v(atlas), other.v(atlas)))
    
    def +(other: IntFunc) = merge(other)(_+_)
    def -(other: IntFunc) = merge(other)(_-_)
    def |(other: IntFunc) = merge(other)(_|_)
    def mask(high: Int, low: Int) =
      new LongFunc((atlas) => v(atlas).mask(high, low))
    def <<(shift: Int) = new LongFunc((atlas) => v(atlas) << shift)
    
    def withRequire(req: Int => Boolean) =
      new LongFunc((atlas) => {
        val result = v(atlas)
        require(req(result))
        result
      })
    
    def generator: BlockGenerator = new IntBlockGenerator(v)
  }
  
  object IntFunc {
    def fromConst(i: Int) = new IntFunc(_ => i)
  }
  
  class LongFunc(val v: MarkAtlas => Long) extends AnyVal {
    private def merge(other: LongFunc)(f: (Long, Long) => Long) =
      new LongFunc((atlas) => f(v(atlas), other.v(atlas)))
    
    def +(other: LongFunc) = merge(other)(_+_)
    def -(other: LongFunc) = merge(other)(_-_)
    def |(other: LongFunc) = merge(other)(_|_)
    def mask(high: Int, low: Int) =
      new LongFunc((atlas) => v(atlas).mask(high, low))
    def <<(shift: Int) = new LongFunc((atlas) => v(atlas) << shift)
    
    def withRequire(req: Long => Boolean) =
      new LongFunc((atlas) => {
        val result = v(atlas)
        require(req(result))
        result
      })
    
    def toInt = new IntFunc((atlas) => v(atlas).toInt)
    
    def generator: BlockGenerator = new LongBlockGenerator(v)
    def intGenerator: BlockGenerator = {
      new LongBlockGenerator(v)
    }
  }
  
  object LongFunc {
    def fromMark(m: Mark) = new LongFunc((atlas) => atlas(m))
    def fromConst(i: Long) = new LongFunc(_ => i)
  }
  
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