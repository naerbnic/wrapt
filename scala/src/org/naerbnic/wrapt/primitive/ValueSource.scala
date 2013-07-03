package org.naerbnic.wrapt.primitive

trait ValueSource {
  def getValue(index: Int): Option[WraptValue]
}