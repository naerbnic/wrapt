package org.naerbnic.wrapt.primitive.impl

import org.naerbnic.wrapt.util.Block

trait BlockSource {
  def getBlock(entry: BlockSource.Location): Block
}

object BlockSource {
  sealed trait Location
  case class ExplicitLocation(offset: Long, size: Long) extends Location
  case class ImplicitLocation(offset: Long) extends Location
}