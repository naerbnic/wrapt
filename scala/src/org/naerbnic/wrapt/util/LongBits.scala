package org.naerbnic.wrapt.util

object LongBits {
  implicit class implicitLongBits(val value: Long) extends AnyVal {
    final def mask(high: Int, low: Int) = {
      val bitmask = (1L << high - low) - 1L
      val shiftedBitmask = bitmask << low
      value & (bitmask << low)
    }
    final def bitRange(high: Int, low: Int) = 
      mask(high, low) >>> low
  }
}