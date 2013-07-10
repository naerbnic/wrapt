package org.naerbnic.wrapt.test

import org.scalatest._
import org.naerbnic.wrapt.Block
import java.nio.charset.Charset

class BlockSpec extends FunSpec with BeforeAndAfter {
  import BlockSpec._
  
  var helloWorld: Block = _
  
  before {
    helloWorld = Block.fromArray("Hello, world!\n".getBytes())
  }
  
  describe ("A Block") {
    it ("convert to a byte buffer") {
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
    
    it ("works under concatenation") {
      val apple = "apple"
      val banana = "banana"
      val carrot = "carrot"
      val block1 = BlockSpec.blockFromString(apple)
      val block2 = BlockSpec.blockFromString(banana)
      val block3 = BlockSpec.blockFromString(carrot)
      
      val catBlock = Block.concat(block1, block2, block3)
      
      assert(catBlock.size == apple.length() + banana.length() + carrot.length())
      
      // Single block reads
      assert(blockToString(catBlock.getSubBlock(0, apple.length())) == apple)
      assert(
          blockToString(
              catBlock.getSubBlock(apple.length() + banana.length(),
              carrot.length())) == carrot)
      
      // Cross block reads
      assert(blockToString(catBlock.getSubBlock(3, 5)) == "leban")
      assert(blockToString(catBlock.getSubBlock(8, 6)) == "anacar")
      
      // Multi-block reads
      assert(blockToString(catBlock) == "applebananacarrot")
    }
  }
}

object BlockSpec {
  def blockFromString(str: String) =
    Block.fromArray(str.getBytes())
  
  def blockToString(b: Block) =
    Charset.forName("UTF-8").decode(b.asByteBuffer()).toString()
}