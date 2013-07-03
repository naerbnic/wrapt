package org.naerbnic.wrapt.primitive

import org.naerbnic.wrapt.Block

trait WraptMap {

}

object WraptMap {
  def fromBlock(block: Block) = {
    new WraptMap() {}
  }
}