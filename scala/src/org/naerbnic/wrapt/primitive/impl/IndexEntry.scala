package org.naerbnic.wrapt.primitive.impl

import org.naerbnic.wrapt.util.LongBits._
import org.naerbnic.wrapt.util.Block
import java.nio.charset.Charset
import org.naerbnic.wrapt.{
  StringValue, BlobValue, IntValue, FloatValue, NullValue, BoolValue
}

import org.naerbnic.wrapt.primitive.{
  PrimValue, PrimBasicValue, PrimArrayValue, PrimMapValue
}

sealed trait IndexEntry

case class BlockIndexEntry(
    location: BlockSource.Location,
    entryType: BlockIndexEntry.Type)
    extends IndexEntry

object BlockIndexEntry {
  sealed trait Type {
    def getValue(block: Block): PrimValue
  }
  
  object Type {
    object BLK_STRING extends Type {
      def getValue(block: Block) = {
        val charbuf = Charset.forName("UTF-8").decode(block.asByteBuffer())
        PrimBasicValue(StringValue(charbuf.toString()))
      }
    }
    
    object BLK_MAP extends Type {
      def getValue(block: Block) = PrimMapValue(null)
    }
    
    object BLK_ARRAY extends Type {
      def getValue(block: Block) = PrimArrayValue(null)
    }
    
    object BLK_BLOB extends Type {
      def getValue(block: Block) = PrimBasicValue(BlobValue(block))
    }
    
    object BLK_INT extends Type {
      def getValue(block: Block) = PrimBasicValue(IntValue(block.readLong(0)))
    }
    
    object BLK_FLOAT extends Type {
      def getValue(block: Block) =
        PrimBasicValue(
            FloatValue(java.lang.Double.longBitsToDouble(block.readLong(0))))
    }
  }
}

case class LiteralIndexEntry(data: Long, entryType: LiteralIndexEntry.Type)
    extends IndexEntry

object LiteralIndexEntry {
  sealed trait Type {
    def getValue(data: Long): PrimValue
  }
  
  object Type {
    object LIT_NULL extends Type {
      def getValue(data: Long) = {
        require (data == 0)
        PrimBasicValue(NullValue)
      }
    }
    
    object LIT_INT extends Type {
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
    
    object LIT_FLOAT extends Type {
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
    
    object LIT_BOOL extends Type {
      def getValue(data: Long) = {
        require (data.mask(60, 1) == 0)
        PrimBasicValue(BoolValue(data.mask(1, 0) != 0))
      }
    }
  }
}

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
        case 0 => Some(BlockIndexEntry.Type.BLK_STRING)
        case 1 => Some(BlockIndexEntry.Type.BLK_MAP)
        case 2 => Some(BlockIndexEntry.Type.BLK_ARRAY)
        case 3 => Some(BlockIndexEntry.Type.BLK_BLOB)
        case 4 => Some(BlockIndexEntry.Type.BLK_INT)
        case 5 => Some(BlockIndexEntry.Type.BLK_FLOAT)
        case _ => None
      }
      
      for (t <- optType) yield BlockIndexEntry(loc, t)
    } else {
      val data = entry.mask(60, 0)
        
      val optType = entry.bitRange(63, 60) match {
        case 0 => Some(LiteralIndexEntry.Type.LIT_NULL)
        case 1 => Some(LiteralIndexEntry.Type.LIT_INT)
        case 2 => Some(LiteralIndexEntry.Type.LIT_FLOAT)
        case 3 => Some(LiteralIndexEntry.Type.LIT_BOOL)
        case _ => None
      }
      
      for (t <- optType) yield LiteralIndexEntry(data, t)
    }
  }
}