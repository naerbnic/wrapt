package org.naerbnic.wrapt.test

import org.scalatest._
import org.naerbnic.wrapt.Block
import java.nio.charset.Charset

class BlockSpec extends FunSpec with BeforeAndAfter {
  var helloWorld: Block = _
  
  before {
    helloWorld = Block.fromArray("Hello, world!\n".getBytes())
  }
  
  describe ("A Block") {
    it ("be convertable to a byte buffer") {
      assert(helloWorld.size == 14)
      val firstBlock = helloWorld.getSubBlock(0, 6)
      
      assert(BlockSpec.blockToString(firstBlock) == "Hello,")
      val secondBlock = helloWorld.getSubBlock(6)
      assert(BlockSpec.blockToString(secondBlock) == " world!\n")
    }
    
    it ("fail with incorrect bounds") {
      val block = Block.fromArray("Hello, world!\n".getBytes())
      intercept[IllegalArgumentException] {
        helloWorld.getSubBlock(6, 12)
      }
    }
    
    it ("reifies to same data") {
      val newBlock = helloWorld.reify()
      assert(BlockSpec.blockToString(newBlock) == "Hello, world!\n")
    }
  }
}

object BlockSpec {
  def blockFromString(str: String) =
    Block.fromArray(str.getBytes())
  
  def blockToString(b: Block) =
    Charset.forName("UTF-8").decode(b.asByteBuffer()).toString()
}