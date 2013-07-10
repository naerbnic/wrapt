package org.naerbnic.wrapt.primitive

import scala.collection.immutable.SortedSet

trait PrimFile {
  def indexes: SortedSet[Int]
  def getValue(index: Int): Option[PrimValue]
}

object PrimFile {
  def fromMap(map: Map[Int, PrimValue]) = new PrimFile {
    def indexes = SortedSet.empty[Int] ++ map.keys
    def getValue(index: Int) = map.get(index)
  }
}