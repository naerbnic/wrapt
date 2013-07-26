package org.naerbnic.wrapt.serialize

import scala.collection.SortedMap
import org.naerbnic.wrapt.util.serializer.FileEntity
import org.naerbnic.wrapt.util.serializer.FileComponent
import org.naerbnic.wrapt.util.serializer.IntFunc
import org.naerbnic.wrapt.util.serializer.Mark

class IndexSerializer(entries: SortedMap[Int, ValueSerializer]) {
  val indexMark = new Mark()
  
  private case class IndexRange(begin: Int, end: Int) {
    def size = end - begin + 1
  }
  
  private def consecutiveRanges = {
    val rangeBuilder = Seq.newBuilder[IndexRange]
    var indexStart = -1
    var indexEnd = -1
    
    for ((index, _) <- entries) {
      if (indexStart < 0) {
        indexStart = index
        indexEnd = index
      } else if (indexEnd + 1 == index) {
        indexEnd = index
      } else {
        rangeBuilder += IndexRange(indexStart, indexEnd)
        indexStart = index
        indexEnd = index
      }
    }
    
    if (indexStart >= 0) {
      rangeBuilder += IndexRange(indexStart, indexEnd)
    }
    
    rangeBuilder.result()
  }
  
  private case class MetadataEntry(
      size: Int, physicalStart: Int, logicalStart: Int) {
    def toEntity = FileEntity.concat(2,
        Seq(FileComponent.ofGen(IntFunc.fromConst(logicalStart).generator),
            FileComponent.ofGen(IntFunc.fromConst(physicalStart).generator),
            FileComponent.ofGen(IntFunc.fromConst(size).generator)))
  }
      
  private lazy val metadataEntries = {
    val metadataBuilder = Seq.newBuilder[MetadataEntry]
    var currPhysical = 0
    
    for (range <- consecutiveRanges) {
      metadataBuilder += MetadataEntry(range.size, currPhysical, range.end)
      currPhysical += range.size
    }
    
    metadataBuilder.result()
  }
  
  private lazy val metadataSize = metadataEntries.size
  
  def metadataTable = {
    FileEntity.concat(2, 
        metadataEntries map (entry => FileComponent.ofEntity(entry.toEntity)))
  }
  
  def indexTable = {
    val components = entries.values map { serializer =>
      FileComponent.ofEntity(serializer.indexEntry)
    }
      
    FileEntity.concat(3, components.toSeq)
  }
  
  def indexEntity = {
    println(metadataTable)
    FileEntity.concat(3,
        Seq(FileComponent.ofMark(indexMark),
            FileComponent.ofGen(IntFunc.fromConst(metadataSize).generator),
            FileComponent.ofGen(IntFunc.fromConst(entries.size).generator),
            FileComponent.ofEntity(metadataTable),
            FileComponent.ofAlign(3),
            FileComponent.ofEntity(indexTable)))
  }
  
  def dataEntity(dataMark: Mark) = {
    val dataSegments = entries.values map { serializer =>
      FileEntity.concat(3,
          Seq(FileComponent.ofEntity(serializer.blockContents),
              FileComponent.ofAlign(3)))
    }
    
    FileEntity.concat(3,
        Seq(FileComponent.ofMark(dataMark)) ++
            (dataSegments map { FileComponent.ofEntity(_) }))
  }
}