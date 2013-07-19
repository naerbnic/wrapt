package org.naerbnic.wrapt.primitive

import org.naerbnic.wrapt.util.Block
import scala.collection.immutable.SortedSet

trait PrimMap {
  def fields: SortedSet[String]
  def getValue(field: String): Option[Int]
}

object PrimMap {
  def fromMap(map: Map[String, Int]) = new PrimMap {
    override lazy val fields = SortedSet.empty[String] ++ map.keys
    override def getValue(field: String) = map.get(field)
  }
}