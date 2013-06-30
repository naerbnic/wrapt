package org.naerbnic.wrapt

import java.nio.ByteBuffer

case class MetaIndexEntry(virtualStart: Int, physicalStart: Int, size: Int) {
  def virtualEnd = virtualStart + size
  def physicalEnd = physicalStart + size
  
  def search(virtualOffset: Int) =
    if (virtualStart > virtualOffset) {
      MetaIndexEntry.Lower
    } else if (virtualEnd <= virtualOffset) {
      MetaIndexEntry.Higher
    } else {
      MetaIndexEntry.Found(virtualOffset - virtualStart + physicalStart)
    }
}

object MetaIndexEntry {
  val Size = 12
  
  def fromBuffer(buffer: ByteBuffer) = {
    val intBuffer = buffer.asIntBuffer()
    val virtualStart = intBuffer.get()
    val physicalStart = intBuffer.get()
    val size = intBuffer.get()
    MetaIndexEntry(virtualStart, physicalStart, size)
  }
  
  sealed trait SearchResult
  object Lower extends SearchResult
  object Higher extends SearchResult
  case class Found(physicalOffset: Int) extends SearchResult
}