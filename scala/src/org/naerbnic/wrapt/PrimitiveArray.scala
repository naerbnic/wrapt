package org.naerbnic.wrapt

class PrimitiveArray(block: Block) {
  require ((block.size % 4) == 0)
  def size = block.size / 4
  
  def get(index: Long) = {
    0
  }
}