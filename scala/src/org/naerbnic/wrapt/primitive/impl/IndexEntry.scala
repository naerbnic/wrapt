package org.naerbnic.wrapt.primitive.impl

import org.naerbnic.wrapt.util.LongBits._
import org.naerbnic.wrapt.util.Block
import java.nio.charset.Charset
import org.naerbnic.wrapt.{
  StringValue, BlobValue, IntValue, FloatValue, NullValue, BoolValue
}
import org.naerbnic.wrapt.primitive.LitIndexType
import org.naerbnic.wrapt.primitive.BlockIndexType

import org.naerbnic.wrapt.primitive.{
  PrimValue, PrimBasicValue, PrimArrayValue, PrimMapValue
}

sealed trait IndexEntry

case class BlockIndexEntry(
    location: BlockSource.Location,
    entryType: BlockIndexType)
    extends IndexEntry

object BlockIndexEntry {
}

case class LiteralIndexEntry(data: Long, entryType: LitIndexType)
    extends IndexEntry

object IndexEntry {
  val Size = 8
  
  def apply(entry: Long) = {
    if (entry.bitRange(64, 63) != 0) {
      val lit_size = entry.bitRange(63, 48).toInt
      val offset = entry.mask(48, 3)
      val loc = if (lit_size == 0)
        BlockSource.ImplicitLocation(offset)
      else
        BlockSource.ExplicitLocation(offset, lit_size)
        
      val optType = entry.bitRange(3, 0) match {
        case 0 => Some(BlockIndexType.BLK_STRING)
        case 1 => Some(BlockIndexType.BLK_MAP)
        case 2 => Some(BlockIndexType.BLK_ARRAY)
        case 3 => Some(BlockIndexType.BLK_BLOB)
        case 4 => Some(BlockIndexType.BLK_INT)
        case 5 => Some(BlockIndexType.BLK_FLOAT)
        case _ => None
      }
      
      for (t <- optType) yield BlockIndexEntry(loc, t)
    } else {
      val data = entry.mask(60, 0)
        
      val optType = entry.bitRange(63, 60) match {
        case 0 => Some(LitIndexType.LIT_NULL)
        case 1 => Some(LitIndexType.LIT_INT)
        case 2 => Some(LitIndexType.LIT_FLOAT)
        case 3 => Some(LitIndexType.LIT_BOOL)
        case _ => None
      }
      
      for (t <- optType) yield LiteralIndexEntry(data, t)
    }
  }
}