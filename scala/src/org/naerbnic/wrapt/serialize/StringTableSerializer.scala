package org.naerbnic.wrapt.serialize

import org.naerbnic.wrapt.util.serializer.Mark
import scala.collection.immutable.SortedSet
import org.naerbnic.wrapt.util.serializer.BlockGenerator
import org.naerbnic.wrapt.util.Composite
import org.naerbnic.wrapt.util.serializer.FileEntity
import org.naerbnic.wrapt.util.serializer.MarkEntity
import java.nio.charset.Charset
import org.naerbnic.wrapt.util.Block
import org.naerbnic.wrapt.util.serializer.BlockEntity
import org.naerbnic.wrapt.util.serializer.BlockGenerator.LongFunc

class StringTableSerializer private (
    val contents: Composite[FileEntity],
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
    val contentBuilder = Seq.newBuilder[FileEntity]
    val stringMarkBuilder = Map.newBuilder[String, Mark]
    
    contentBuilder += MarkEntity(tableMark)
    
    for (str <- sortedStrings) {
      val stringMark = new Mark()
      stringMarkBuilder += (str -> stringMark)
      contentBuilder += MarkEntity(stringMark)
      contentBuilder += BlockEntity(
          BlockGenerator.fromBlock(stringToBlock(str)))
    }
    
    new StringTableSerializer(
        Composite(contentBuilder.result(): _*),
        tableMark,
        stringMarkBuilder.result())
  }
}