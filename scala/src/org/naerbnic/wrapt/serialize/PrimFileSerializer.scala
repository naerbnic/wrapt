package org.naerbnic.wrapt.serialize

import org.naerbnic.wrapt.util.Block
import org.naerbnic.wrapt.primitive.PrimFile
import org.naerbnic.wrapt.primitive.PrimValue
import org.naerbnic.wrapt.primitive.PrimMapValue
import org.naerbnic.wrapt.util.serializer.Mark
import org.naerbnic.wrapt.util.serializer.FileComponent
import org.naerbnic.wrapt.util.serializer.FileEntity
import org.naerbnic.wrapt.util.serializer.IntFunc
import org.naerbnic.wrapt.util.serializer.LongFunc
import scala.collection.SortedMap
import org.naerbnic.wrapt.util.serializer.BlockGenerator

object PrimFileSerializer {
  def valueToTableStrings(value: PrimValue) = {
    value match {
      case PrimMapValue(map) => map.fields
      case _ => Set.empty[String]
    }
  }
  
  def header(
      stringTableMark: Mark,
      dataMark: Mark) = {
    FileEntity.concat(3,
        Seq(FileComponent.ofGen(BlockGenerator.fromLong(0L)),
            FileComponent.ofGen(stringTableMark.posFunc.generator),
            FileComponent.ofGen(dataMark.posFunc.generator)))
  }
  
  def serialize(file: PrimFile): FileEntity = {
    val tableStrings = (for {
      index <- file.indexes
      value <- file.getValue(index)
      strings = valueToTableStrings(value)
    } yield strings) . flatten
    
    val stringTable = StringTableSerializer(tableStrings)
    
    val dataSegmentMark = new Mark()
    
    val valueSerializerPairs = for {
      index <- file.indexes
      value <- file.getValue(index)
    } yield (index -> ValueSerializer(dataSegmentMark, stringTable, value))
    
    val indexSerializer = new IndexSerializer(
        SortedMap.empty[Int, ValueSerializer] ++ valueSerializerPairs.toMap)
    
    val headerEntity = header(stringTable.tableMark, dataSegmentMark)
    
    FileEntity.concat(3,
        Seq(FileComponent.ofEntity(headerEntity),
            FileComponent.ofEntity(indexSerializer.indexEntity),
            FileComponent.ofEntity(stringTable.contents),
            FileComponent.ofEntity(indexSerializer.dataEntity(dataSegmentMark))))
  }
}