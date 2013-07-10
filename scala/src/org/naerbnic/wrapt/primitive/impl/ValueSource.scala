package org.naerbnic.wrapt.primitive.impl

import org.naerbnic.wrapt.primitive.PrimValue

trait ValueSource {
  def getValue(index: Int): Option[PrimValue]
}