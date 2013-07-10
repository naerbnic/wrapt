package org.naerbnic.wrapt.serialize

import org.naerbnic.wrapt.util.Composite
import org.naerbnic.wrapt.Block

object Serializer {
  private def findMarkOffsets(entitySeq: Seq[FileEntity]) = {
    val mapBuilder = Map.newBuilder[Mark, Long]
    var currPos = 0L
    
    for (entity <- entitySeq) {
      entity match {
        case MarkEntity(m) => {
          mapBuilder += (m -> currPos)
        }
        
        case BlockEntity(gen) => {
          currPos += gen.size
        }
      }
    }
    
    mapBuilder.result()
  }
  
  def apply(contents: Composite[FileEntity]) = {
    val entitySeq = contents.asSeq
    val markOffsets = findMarkOffsets(entitySeq)
    
    val blockOpts = for {
      entity <- entitySeq
    } yield entity match {
      case MarkEntity(_) => None
      case BlockEntity(gen) => Some(gen.createBlock(markOffsets))
    }
    
    val blocks = blockOpts.flatten
    
    Block.concat(blocks: _*)
  }
}