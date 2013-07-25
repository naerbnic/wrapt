package org.naerbnic.wrapt.serialize

import org.naerbnic.wrapt.util.serializer.Mark
import scala.collection.immutable.SortedSet
import org.naerbnic.wrapt.util.serializer.BlockGenerator
import org.naerbnic.wrapt.util.Composite
import org.naerbnic.wrapt.util.serializer.FileEntity
import java.nio.charset.Charset
import org.naerbnic.wrapt.util.Block
import org.naerbnic.wrapt.util.serializer.BlockGenerator.LongFunc
import org.naerbnic.wrapt.util.serializer.FileComponent

class StringTableSerializer private (
    val contents: FileEntity,
    val tableMark: Mark,
    stringMarks: Map[String, Mark]) {
  def getEntryFunc(string: String) = {
    val stringPos = LongFunc.fromMark(stringMarks(string))
    (stringPos - LongFunc.fromMark(tableMark)).toInt
  }
}

object StringTableSerializer {
  def stringToBlock(str: String) =
    Block.fromBuffer(Charset.forName("UTF-8").encode(str))
  
  def apply(strings: Iterable[String]) = {
    val sortedStrings = SortedSet.empty[String] ++ strings
    
    val tableMark = new Mark()
    val contentBuilder = Seq.newBuilder[FileComponent]
    val stringMarkBuilder = Map.newBuilder[String, Mark]
    
    contentBuilder += FileComponent.ofMark(tableMark)
    
    for (str <- sortedStrings) {
      val stringMark = new Mark()
      stringMarkBuilder += (str -> stringMark)
      
      contentBuilder += FileComponent.ofMark(stringMark)
      contentBuilder += FileComponent.ofBlock(stringToBlock(str))
    }
    
    new StringTableSerializer(
        FileEntity.concat(0, contentBuilder.result()),
        tableMark,
        stringMarkBuilder.result())
  }
}