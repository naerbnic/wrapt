package org.naerbnic.wrapt.primitive

trait PrimArray {
  def size: Int
  def getValue(index: Int): Option[Int]
}

object PrimArray {
  def fromSeq(seq: Seq[Int]) = new PrimArray {
    override def size = seq.size
    override def getValue(index: Int) = {
      if (index < 0) {
        None
      } else if (index >= seq.size) {
        None
      } else {
        Some(seq(index.toInt))
      }
    }
  }
}