package org.naerbnic.wrapt.primitive

trait PrimArray {
  def size: Long
  def getValue(index: Long): Option[PrimValue]
}

object PrimArray {
  def fromSeq(seq: Seq[PrimValue]) = new PrimArray {
    override def size = seq.size
    override def getValue(index: Long) = {
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