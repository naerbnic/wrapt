package org.naerbnic.wrapt.primitive.impl

import org.naerbnic.wrapt.util.Block
import org.naerbnic.wrapt.primitive.PrimFile

class PrimFileImpl private (
    header: Header,
    stringTable: StringTable,
    index: Index,
    blockSource: BlockSource) extends PrimFile {
  def indexes = index.entryIndexes
  def getValue(indexOffset: Int) = {
    for (entry <- index.indexEntry(indexOffset))
    yield entry match {
      case BlockIndexEntry(loc, t) => t.getValue(blockSource.getBlock(loc))
      case LiteralIndexEntry(data, t) => t.getValue(data)
    }
  }
}

object PrimFileImpl {
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
  
  def fromBlock(block: Block) = {
    for {
      header <- Header.fromBlock(block)
      val stringTable = StringTable.fromFile(block, header.stringTableOffset)
      val index = Index.fromFile(block, Header.Size)
      val dataSegment = block.getSubBlock(header.dataOffset)
    } yield new PrimFileImpl(header, stringTable, index, 
        new WraptFileBlockSource(dataSegment))
  }
}