package org.naerbnic.wrapt.primitive

import org.naerbnic.wrapt.util.LongBits._

sealed trait IndexEntry

case class BlockIndexEntry(size: Option[Int], offset: Long, entryType: BlockIndexEntry.Type) extends IndexEntry

object BlockIndexEntry {
  type Type = Type.T
  object Type extends Enumeration {
    type T = Value
    val BLK_STRING, BLK_MAP, BLK_ARRAY, BLK_BLOB, BLK_INT, BLK_FLOAT = Value
  }
}

case class LiteralIndexEntry(data: Long, entryType: LiteralIndexEntry.Type) extends IndexEntry {
  def literalInt =
    if (entryType == LiteralIndexEntry.Type.LIT_INT) {
      val base = data.mask(59, 0)
      if (data.bitRange(60, 59) == 0) {
        Some(base)
      } else {
        // Sign Extend
        Some((0x1fL << 59) | base)
      }
    } else {
      None
    }
  
  def literalFloat = 
    if (entryType == LiteralIndexEntry.Type.LIT_FLOAT) {
      val signBit = data.bitRange(60, 59)
      val exponent = data.bitRange(59, 52)
      val mantissa = data.bitRange(52, 0)
      
      val newExponent = exponent + (2 << 6) - (2 << 10)
      
      val newDoubleBits =
        (signBit << 63) | (exponent << 52) | mantissa
        
      Some(java.lang.Double.longBitsToDouble(newDoubleBits))
    } else {
      None
    }
  
  def literalBool =
    if (entryType == IndexEntry.Type.BOOL) {
      Some(data.mask(1, 0) != 0) 
    } else {
      None
    }
  
  def isLiteralNull = entryType == IndexEntry.Type.NULL
}

object LiteralIndexEntry {
  type Type = Type.T
  object Type extends Enumeration {
    type T = Value
    val LIT_NULL, LIT_INT, LIT_FLOAT, LIT_BOOL = Value
  }
}

/*
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
        case 3 => Some(IndexEntry.Type.BOOL)
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
    
  def asLiteralEntry = {
    val entryType = this.entryType
    if (isLiteral && entryType.nonEmpty) {
      Some(LiteralIndexEntry(mask(60, 0), entryType.get))
    } else {
      None
    }
  }
    
  def asBlockEntry = {
    val entryType = this.entryType
    if (!isLiteral && entryType.nonEmpty) {
      val baseSize = bitRange(63, 48).toInt
      val blockSize = if (baseSize == 0) None else Some(baseSize)
      Some(BlockIndexEntry(blockSize, mask(48, 3), entryType.get))
    } else {
      None
    }
  }
}
*/
object IndexEntry {
  val Size = 8
  
  def apply(entry: Long) = {
    if (entry.bitRange(64, 63) != 0) {
      val lit_size = entry.bitRange(63, 48).toInt
      val size = if (lit_size == 0) None else Some(lit_size)
      val optType = entry.bitRange(3, 0) match {
        case 0 => Some(BlockIndexEntry.Type.BLK_STRING)
        case 1 => Some(BlockIndexEntry.Type.BLK_MAP)
        case 2 => Some(BlockIndexEntry.Type.BLK_ARRAY)
        case 3 => Some(BlockIndexEntry.Type.BLK_BLOB)
        case 4 => Some(BlockIndexEntry.Type.BLK_INT)
        case 5 => Some(BlockIndexEntry.Type.BLK_FLOAT)
        case _ => None
      }
      
      for (t <- optType) yield BlockIndexEntry(size, entry.mask(48, 3), t)
    } else {
      
    }
  }
}