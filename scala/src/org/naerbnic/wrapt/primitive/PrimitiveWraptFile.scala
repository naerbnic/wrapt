package org.naerbnic.wrapt.primitive

import java.nio.channels.FileChannel
import java.nio.ByteBuffer
import org.naerbnic.wrapt.Block

class PrimitiveWraptFile private (
    header: Header,
    stringTable: StringTable,
    index: Index,
    blockSource: BlockSource) extends ValueSource {
  def getValue(indexOffset: Int) = {
    for (entry <- index.indexEntry(indexOffset))
    yield entry match {
      case BlockIndexEntry(loc, t) => t.getValue(blockSource.getBlock(loc))
      case LiteralIndexEntry(data, t) => t.getValue(data)
    }
  }
}

object PrimitiveWraptFile {
  private class WraptFileBlockSource(dataSegment: Block) extends BlockSource {
    def getBlock(entry: BlockSource.Location) =
      entry match {
        case BlockSource.ExplicitLocation(offset, size) => 
          dataSegment.getSubBlock(offset, size)
          
        case BlockSource.ImplicitLocation(offset) => {
          val size = dataSegment.readLong(offset)
          dataSegment.getSubBlock(offset + 8, size)
        }
      }
  }
  
  def fromFile(block: Block) = {
    for {
      header <- Header.fromBlock(block)
      val stringTable = StringTable.fromFile(block, header.stringTableOffset)
      val index = Index.fromFile(block, Header.Size)
      val dataSegment = block.getSubBlock(header.dataOffset)
    } yield new PrimitiveWraptFile(header, stringTable, index, 
        new WraptFileBlockSource(dataSegment))
  }
}