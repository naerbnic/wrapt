package org.naerbnic.wrapt.util.serializer

import org.naerbnic.wrapt.util.LongBits._
import java.nio.ByteBuffer
import org.naerbnic.wrapt.util.Block
  
class IntFunc(val v: BlockGenerator.MarkAtlas => Int) extends AnyVal {
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
  
  def generator: BlockGenerator = new IntFunc.IntBlockGenerator(v)
}

object IntFunc {
  def fromConst(i: Int) = new IntFunc(_ => i)
  
  private class IntBlockGenerator(f: BlockGenerator.MarkAtlas => Int)
      extends BlockGenerator {
    override def size = java.lang.Integer.SIZE / java.lang.Byte.SIZE
    def createBlock(atlas: BlockGenerator.MarkAtlas) = {
      val buffer = ByteBuffer.allocate(size.toInt)
      buffer.asIntBuffer().put(0, f(atlas))
      Block.fromBuffer(buffer)
    }
  }
}