package org.naerbnic.wrapt.serialize

import java.nio.CharBuffer
import java.nio.charset.Charset

import org.naerbnic.wrapt.BasicValue.BlobValue
import org.naerbnic.wrapt.BasicValue.BoolValue
import org.naerbnic.wrapt.BasicValue.FloatValue
import org.naerbnic.wrapt.BasicValue.IntValue
import org.naerbnic.wrapt.BasicValue.NullValue
import org.naerbnic.wrapt.BasicValue.StringValue
import org.naerbnic.wrapt.primitive.BlockIndexType
import org.naerbnic.wrapt.primitive.LitIndexType
import org.naerbnic.wrapt.primitive.PrimArray
import org.naerbnic.wrapt.primitive.PrimArrayValue
import org.naerbnic.wrapt.primitive.PrimBasicValue
import org.naerbnic.wrapt.primitive.PrimMap
import org.naerbnic.wrapt.primitive.PrimMapValue
import org.naerbnic.wrapt.primitive.PrimValue
import org.naerbnic.wrapt.util.Block
import org.naerbnic.wrapt.util.LongBits.implicitLongBits
import org.naerbnic.wrapt.util.serializer.BlockGenerator
import org.naerbnic.wrapt.util.serializer.IntFunc
import org.naerbnic.wrapt.util.serializer.LongFunc
import org.naerbnic.wrapt.util.serializer.FileComponent
import org.naerbnic.wrapt.util.serializer.FileEntity
import org.naerbnic.wrapt.util.serializer.Mark

trait ValueSerializer {
  def indexEntry: FileEntity
  def blockContents: FileEntity
}

object ValueSerializer {
  class LiteralValueSerializer(data: Long, ty: LitIndexType) 
      extends ValueSerializer {
    override def blockContents = FileEntity.ofBlock(Block.zeroes(0L))
    override def indexEntry = {
      val entry = (1L << 63) | (ty.code << 60) | data 
      println(f"${ty.code}%x, $entry%x, $data%x")
      FileEntity.ofGen(BlockGenerator.fromLong(entry))
    }
  }
  
  abstract class BlockValueSerializer(
      contents: FileEntity,
      ty: BlockIndexType,
      dataSegmentMark: Mark) extends ValueSerializer {
    val size = contents
    val blockMark = new Mark()
  }
  
  class ImplicitBlockValueSerializer(
      contents: FileEntity,
      ty: BlockIndexType,
      dataSegmentMark: Mark)
      extends BlockValueSerializer(contents, ty, dataSegmentMark) {
    require (contents.size != 0 && contents.size < (1L << 15))
    
    override def blockContents = {
      FileEntity.concat(3,
          Seq(FileComponent.ofMark(blockMark),
              FileComponent.ofEntity(contents)))
    }
    
    override def indexEntry = {
      val sizeBits = LongFunc.fromConst(contents.size) << 48
      val offset = blockMark.posFunc - dataSegmentMark.posFunc
      val offsetBits = offset.mask(48, 3)
      val typeBits = LongFunc.fromConst(ty.code)
      
      FileEntity.ofGen((sizeBits | offsetBits | typeBits).generator)
    }
  }
  
  class ExplicitBlockValueSerializer(
      contents: FileEntity,
      ty: BlockIndexType,
      dataSegmentMark: Mark)
      extends BlockValueSerializer(contents, ty, dataSegmentMark) {
    
    override def blockContents = {
      FileEntity.concat(3, 
          Seq(FileComponent.ofMark(blockMark),
              FileComponent.ofGen(LongFunc.fromConst(contents.size).generator),
              FileComponent.ofEntity(contents)))
    }
    
    override def indexEntry = {
      val offset = blockMark.posFunc - dataSegmentMark.posFunc
      val offsetBits = offset.mask(48, 3)
      val typeBits = LongFunc.fromConst(ty.code)
      
      FileEntity.ofGen((offsetBits | typeBits).generator)
    }
  }
  
  def createBlockValueSerializer(
      contents: FileEntity,
      ty: BlockIndexType,
      dataSegmentMark: Mark) = {
    val size = contents.size
    if (size != 0 && size < (1L << 15)) {
      new ImplicitBlockValueSerializer(contents, ty, dataSegmentMark)
    } else {
      new ExplicitBlockValueSerializer(contents, ty, dataSegmentMark)
    }
  }
  
  def apply(
      dataSegmentMark: Mark,
      stringTable: StringTableSerializer,
      value: PrimValue) = {
    value match {
      case PrimBasicValue(NullValue) =>
        new LiteralValueSerializer(0L, LitIndexType.LIT_NULL)
        
      case PrimBasicValue(IntValue(i)) => {
        val highBits = i.bitRange(64, 59) // Include the sign bit
        if (highBits == 0L || highBits == 0x1FL) {
          new LiteralValueSerializer(i.mask(60, 0), LitIndexType.LIT_INT)
        } else {
          new ImplicitBlockValueSerializer(
              FileEntity.ofGen(BlockGenerator.fromLong(i)),
              BlockIndexType.BLK_INT,
              dataSegmentMark)
        }
      }
      
      case PrimBasicValue(FloatValue(f)) => {
        // FIXME: Just for now, encode all floats as block data.
        val floatBits = java.lang.Double.doubleToLongBits(f)
        new ImplicitBlockValueSerializer(
            FileEntity.ofGen(BlockGenerator.fromLong(floatBits)),
            BlockIndexType.BLK_INT,
            dataSegmentMark)
      }
      
      case PrimBasicValue(BoolValue(b)) => {
        new LiteralValueSerializer(if(b) 1L else 0L, LitIndexType.LIT_BOOL)
      }
      
      case PrimBasicValue(StringValue(str)) => {
        val bytes =
          Charset.forName("UTF-8").encode(CharBuffer.wrap(str.toCharArray()))
        val contents = FileEntity.ofBlock(Block.fromBuffer(bytes))
        createBlockValueSerializer(
            contents, BlockIndexType.BLK_STRING, dataSegmentMark)
      }
      
      case PrimBasicValue(BlobValue(blob)) => {
        val contents = FileEntity.ofBlock(blob)
        createBlockValueSerializer(
            contents, BlockIndexType.BLK_BLOB, dataSegmentMark)
      }
      
      case PrimMapValue(map) => {
        createBlockValueSerializer(
            createMapContents(stringTable, map),
            BlockIndexType.BLK_MAP,
            dataSegmentMark)
      }
      case PrimArrayValue(array) => {
        createBlockValueSerializer(
            createArrayContents(array),
            BlockIndexType.BLK_ARRAY,
            dataSegmentMark)
      }
    }
  }
  
  def createMapContents(table: StringTableSerializer, map: PrimMap) = {
    val entries = for {
      k <- map.fields.view.toSeq
      v = map.getValue(k).get
    } yield FileEntity.concat(0,
        Seq(FileComponent.ofGen(table.getEntryFunc(k).generator),
            FileComponent.ofGen(IntFunc.fromConst(v).generator)))
    FileEntity.concat(0, entries map (FileComponent.ofEntity(_)))
  }
  
  def createArrayContents(array: PrimArray) = {
    val entries = for {
      i <- 0 until array.size
      v = array.getValue(i).get
    } yield FileComponent.ofGen(IntFunc.fromConst(v).generator)
    
    FileEntity.concat(0, entries)
  }
}