package org.naerbnic.wrapt.primitive

import org.naerbnic.wrapt.util.LongBits._
import org.naerbnic.wrapt.BasicValue._

sealed abstract class LitIndexType(val code: Long) {
  def getValue(data: Long): PrimValue
}

object LitIndexType {
  object LIT_NULL extends LitIndexType(0x0) {
    def getValue(data: Long) = {
      require (data == 0)
      PrimBasicValue(NullValue)
    }
  }
  
  object LIT_INT extends LitIndexType(0x1) {
    def getValue(data: Long) = {
      val base = data.mask(59, 0)
      val result = if (data.bitRange(60, 59) == 0) {
        base
      } else {
        // Sign Extend
        (0x1fL << 59) | base
      }
      
      PrimBasicValue(IntValue(result))
    }
  }
  
  object LIT_FLOAT extends LitIndexType(0x2) {
    def getValue(data: Long) = {
      val signBit = data.bitRange(60, 59)
      val exponent = data.bitRange(59, 52)
      val mantissa = data.bitRange(52, 0)
      
      val newExponent = exponent + (2 << 6) - (2 << 10)
      
      val newDoubleBits =
        (signBit << 63) | (exponent << 52) | mantissa
        
      PrimBasicValue(
          FloatValue(java.lang.Double.longBitsToDouble(newDoubleBits)))
    }
  }
  
  object LIT_BOOL extends LitIndexType(0x3) {
    def getValue(data: Long) = {
      require (data.mask(60, 1) == 0)
      PrimBasicValue(BoolValue(data.mask(1, 0) != 0))
    }
  }
}