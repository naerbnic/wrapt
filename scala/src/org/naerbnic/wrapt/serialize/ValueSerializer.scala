package org.naerbnic.wrapt.serialize

import org.naerbnic.wrapt.util.Composite
import org.naerbnic.wrapt.util.serializer.FileEntity
import org.naerbnic.wrapt.util.serializer.Mark
import org.naerbnic.wrapt.primitive.PrimValue
import org.naerbnic.wrapt.primitive.PrimBasicValue
import org.naerbnic.wrapt.BasicValue._
import org.naerbnic.wrapt.primitive.PrimMapValue
import org.naerbnic.wrapt.primitive.PrimArrayValue
import org.naerbnic.wrapt.primitive.LitIndexType
import org.naerbnic.wrapt.util.serializer.BlockEntity
import org.naerbnic.wrapt.util.serializer.BlockGenerator
import org.naerbnic.wrapt.primitive.BlockIndexType
import org.naerbnic.wrapt.util.LongBits._
import org.naerbnic.wrapt.util.serializer.BlockEntity
import org.naerbnic.wrapt.util.serializer.MarkEntity
import org.naerbnic.wrapt.util.serializer.BlockEntity
import org.naerbnic.wrapt.util.serializer.AlignEntity
import org.naerbnic.wrapt.util.serializer.Serializer
import org.naerbnic.wrapt.util.serializer.MarkEntity
import org.naerbnic.wrapt.util.serializer.BlockGenerator.LongFunc
import java.nio.charset.Charset
import java.nio.CharBuffer
import org.naerbnic.wrapt.util.Block
import org.naerbnic.wrapt.primitive.PrimMap
import org.naerbnic.wrapt.util.serializer.BlockEntity
import org.naerbnic.wrapt.util.serializer.BlockGenerator.IntFunc
import org.naerbnic.wrapt.primitive.PrimArray
import org.naerbnic.wrapt.util.serializer.BlockEntity

trait ValueSerializer {
  def indexEntry: Composite[FileEntity]
  def blockContents: Composite[FileEntity]
}

object ValueSerializer {
  private def entitySize(entities: Composite[FileEntity], alignment: Int) = {
    entities.asSeq.foldLeft(0L) { (pos, entity) =>
      entity match {
        case BlockEntity(block) => pos + block.size
        case AlignEntity(i) => {
          require (i <= alignment)
          pos + Serializer.getAlignSize(i, pos)
        }
        case MarkEntity(_) => pos
      }
    }
  }
  
  class LiteralValueSerializer(data: Long, ty: LitIndexType) 
      extends ValueSerializer {
    override def blockContents = Composite()
    override def indexEntry = {
      val entry = (1L << 63) | (ty.code << 60) | data 
      Composite(BlockEntity(BlockGenerator.fromLong(entry)))
    }
  }
  
  abstract class BlockValueSerializer(
      contents: Composite[FileEntity],
      ty: BlockIndexType,
      dataSegmentMark: Mark) extends ValueSerializer {
    val size = entitySize(contents, 3)
    val blockMark = new Mark()
  }
  
  class ImplicitBlockValueSerializer(
      contents: Composite[FileEntity],
      ty: BlockIndexType,
      dataSegmentMark: Mark)
      extends BlockValueSerializer(contents, ty, dataSegmentMark) {
    require (size != 0 && size < (1L << 15))
    
    override def blockContents = {
      val markComposite = Composite[FileEntity](MarkEntity(blockMark))
      Composite.concat(markComposite, contents)
    }
    
    override def indexEntry = {
      val sizeBits = LongFunc.fromConst(size) << 48
      val offset =
        LongFunc.fromMark(blockMark) - LongFunc.fromMark(dataSegmentMark)
      val offsetBits = offset.mask(48, 3)
      val typeBits = LongFunc.fromConst(ty.code)
      
      Composite(BlockEntity((sizeBits | offsetBits | typeBits).generator))
    }
  }
  
  class ExplicitBlockValueSerializer(
      contents: Composite[FileEntity],
      ty: BlockIndexType,
      dataSegmentMark: Mark)
      extends BlockValueSerializer(contents, ty, dataSegmentMark) {
    override def blockContents = {
      val markComposite = Composite[FileEntity](MarkEntity(blockMark))
      val sizeComposite =
        Composite[FileEntity](BlockEntity(LongFunc.fromConst(size).generator))
      Composite.concat(markComposite, sizeComposite, contents)
    }
    
    override def indexEntry = {
      val offset =
        LongFunc.fromMark(blockMark) - LongFunc.fromMark(dataSegmentMark)
      val offsetBits = offset.mask(48, 3)
      val typeBits = LongFunc.fromConst(ty.code)
      
      Composite(BlockEntity((offsetBits | typeBits).generator))
    }
  }
  
  def createBlockValueSerializer(
      contents: Composite[FileEntity],
      ty: BlockIndexType,
      dataSegmentMark: Mark) = {
    val size = entitySize(contents, 3)
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
              Composite(BlockEntity(BlockGenerator.fromLong(i))),
              BlockIndexType.BLK_INT,
              dataSegmentMark)
        }
      }
      
      case PrimBasicValue(FloatValue(f)) => {
        // FIXME: Just for now, encode all floats as block data.
        val floatBits = java.lang.Double.doubleToLongBits(f)
        new ImplicitBlockValueSerializer(
            Composite(BlockEntity(BlockGenerator.fromLong(floatBits))),
            BlockIndexType.BLK_INT,
            dataSegmentMark)
      }
      
      case PrimBasicValue(BoolValue(b)) => {
        new LiteralValueSerializer(if(b) 1L else 0L, LitIndexType.LIT_BOOL)
      }
      
      case PrimBasicValue(StringValue(str)) => {
        val bytes =
          Charset.forName("UTF-8").encode(CharBuffer.wrap(str.toCharArray()))
        val contents =
          Composite[FileEntity](
              BlockEntity(BlockGenerator.fromBlock(Block.fromBuffer(bytes))))
        createBlockValueSerializer(
            contents, BlockIndexType.BLK_STRING, dataSegmentMark)
      }
      
      case PrimBasicValue(BlobValue(blob)) => {
        val contents =
          Composite[FileEntity](
              BlockEntity(BlockGenerator.fromBlock(blob)))
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
      val v = map.getValue(k).get
    } yield Composite[FileEntity](
        BlockEntity(table.getEntryFunc(k).generator),
        BlockEntity(IntFunc.fromConst(v).generator))
    Composite.concat[FileEntity](entries : _*)
  }
  
  def createArrayContents(array: PrimArray) = {
    val entries = for {
      i <- 0 until array.size
      val v = array.getValue(i).get
    } yield Composite[FileEntity](
        BlockEntity(IntFunc.fromConst(v).generator))
    Composite.concat[FileEntity](entries : _*)
  }
}