package org.naerbnic.wrapt.serialize

import org.naerbnic.wrapt.util.Block
import org.naerbnic.wrapt.primitive.PrimFile
import org.naerbnic.wrapt.primitive.PrimValue
import org.naerbnic.wrapt.primitive.PrimMapValue

object PrimFileSerializer {
  def valueToTableStrings(value: PrimValue) = {
    value match {
      case PrimMapValue(map) => map.fields
      case _ => Set.empty[String]
    }
  }
  
  def serialize(file: PrimFile): Block = {
    val tableStrings = (for {
      index <- file.indexes
      value <- file.getValue(index)
      val strings = valueToTableStrings(value)
    } yield strings) flatten
    
    val stringTable = StringTableSerializer(tableStrings)
    
    
    Block.NullBlock
  }
}