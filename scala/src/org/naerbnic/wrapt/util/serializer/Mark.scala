package org.naerbnic.wrapt.util.serializer

// A simple class representing a position in a file. Only provides
// object uniqueness
class Mark {
  def posFunc = new LongFunc((atlas) => atlas(this))
}