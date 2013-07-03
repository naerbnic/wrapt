package org.naerbnic.wrapt.util

object LongBits {
  implicit class implicitLongBits(val value: Long) extends AnyVal {
    final def mask(high: Int, low: Int) =
      value & ((1 << (high - low) - 1) << low)
    final def bitRange(high: Int, low: Int) = 
      mask(high, low) >>> low
  }
}