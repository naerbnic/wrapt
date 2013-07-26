package org.naerbnic.wrapt.util.serializer

import org.naerbnic.wrapt.util.LongBits._
import java.nio.ByteBuffer
import org.naerbnic.wrapt.util.Block
  
class LongFunc(val v: BlockGenerator.MarkAtlas => Long) extends AnyVal {
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
  
  def generator: BlockGenerator = new LongFunc.LongBlockGenerator(v)
  def intGenerator: BlockGenerator = toInt.generator
}

object LongFunc {
  def fromConst(i: Long) = new LongFunc(_ => i)
  
  private class LongBlockGenerator(f: BlockGenerator.MarkAtlas => Long)
      extends BlockGenerator {
    override def size = java.lang.Long.SIZE / java.lang.Byte.SIZE
    def createBlock(atlas: BlockGenerator.MarkAtlas) = {
      val buffer = ByteBuffer.allocate(size.toInt)
      buffer.asLongBuffer().put(0, f(atlas))
      Block.fromBuffer(buffer)
    }
  }
}