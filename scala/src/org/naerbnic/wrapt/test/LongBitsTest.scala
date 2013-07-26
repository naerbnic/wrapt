package org.naerbnic.wrapt.test

import org.scalatest._
import org.naerbnic.wrapt.util.LongBits._

class LongBitsTest extends FunSpec {
  describe ("mask") {
    it ("should work on the low bits") {
      val long = 0x0123456789abcdefL
      assert(long.mask(4, 0) === 0xFL)
    }
    
    it ("should work on the high bits") {
      val long = 0x0123456789abcdefL
      assert(long.mask(64, 56) === 0x0100000000000000L)
    }
  }
}