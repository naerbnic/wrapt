package org.naerbnic.wrapt

class IndexEntry(val entry: Long) extends AnyVal {
  private def mask(high: Int, low: Int) =
    entry & ((1 << (high - low) - 1) << low)
  private def bitRange(high: Int, low: Int) = 
    mask(high, low) >>> low
    
  private def isLiteral = bitRange(64, 63) != 0
  private def literalTypeCode = bitRange(63, 60)
  private def blockTypeCode = bitRange(3, 0)
  
  def entryType = 
    if (isLiteral) {
      literalTypeCode match {
        case 0 => Some(IndexEntry.Type.NULL)
        case 1 => Some(IndexEntry.Type.INT)
        case 2 => Some(IndexEntry.Type.FLOAT)
        case 3 | 4 => Some(IndexEntry.Type.BOOL)
        case _ => None
      }
    } else {
      blockTypeCode match {
        case 0 => Some(IndexEntry.Type.STRING)
        case 1 => Some(IndexEntry.Type.MAP)
        case 2 => Some(IndexEntry.Type.ARRAY)
        case 3 => Some(IndexEntry.Type.BLOB)
        case 4 => Some(IndexEntry.Type.INT)
        case 5 => Some(IndexEntry.Type.FLOAT)
      }
    }
  
  def offset = 
    if (isLiteral) {
      None
    } else {
      Some(mask(48, 3))
    }
  
  def blockSize =
    if (isLiteral) {
      None
    } else {
      Some(bitRange(63, 48).toInt)
    }
  
  def literalInt =
    if (isLiteral && entryType == IndexEntry.Type.INT) {
      val base = mask(59, 0)
      if (bitRange(60, 59) == 0) {
        Some(base)
      } else {
        // Sign Extend
        Some((0x1fL << 59) | base)
      }
    } else {
      None
    }
  
  def literalFloat = 
    if (isLiteral && entryType == IndexEntry.Type.FLOAT) {
      val signBit = bitRange(60, 59)
      val exponent = bitRange(59, 52)
      val mantissa = bitRange(52, 0)
      
      val newExponent = exponent + (2 << 6) - (2 << 10)
      
      val newDoubleBits =
        (signBit << 63) | (exponent << 52) | mantissa
        
      Some(java.lang.Double.longBitsToDouble(newDoubleBits))
    } else {
      None
    }
  
  def literalBool =
    if (isLiteral) {
      literalTypeCode match {
        case 3 => Some(false)
        case 4 => Some(true)
        case _ => None
      }
    } else {
      None
    }
  
  def isLiteralNull = isLiteral && entryType == IndexEntry.Type.NULL
}

object IndexEntry {
  val Size = 8
  object Type extends Enumeration {
    type T = Value
    val NULL, BOOL, INT, FLOAT, STRING, MAP, ARRAY, BLOB = Value
  }
  
  type Type = Type.T
  
  def apply(entry: Long) = new IndexEntry(entry)
}