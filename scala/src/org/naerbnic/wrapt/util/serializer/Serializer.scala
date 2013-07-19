package org.naerbnic.wrapt.util.serializer

import org.naerbnic.wrapt.util.Composite
import org.naerbnic.wrapt.util.Block
import scala.Option.option2Iterable

object Serializer {
  def getAlignSize(n: Int, pos: Long) = {
    require (n >= 0)
    require (n < 64)
    
    val bitmask = (1L << n) - 1L
    (- pos) & bitmask
  }
  
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
        
        case AlignEntity(n) => {
          currPos += getAlignSize(n, currPos)
        }
      }
    }
    
    mapBuilder.result()
  }
  
  def apply(contents: Composite[FileEntity]) = {
    val entitySeq = contents.asSeq
    val markOffsets = findMarkOffsets(entitySeq)
    
    var currPos = 0L
    
    val blockOpts = for {
      entity <- entitySeq
    } yield entity match {
      case MarkEntity(_) => None
      case BlockEntity(gen) => {
        currPos += gen.size
        Some(gen.createBlock(markOffsets))
      }
      case AlignEntity(n) => {
        val alignSize = getAlignSize(n, currPos)
        currPos += alignSize
        Some(Block.zeroes(alignSize))
      }
    }
    
    val blocks = blockOpts.flatten
    
    Block.concat(blocks: _*)
  }
}