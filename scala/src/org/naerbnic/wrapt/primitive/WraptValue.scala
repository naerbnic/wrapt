package org.naerbnic.wrapt.primitive

import org.naerbnic.wrapt.Block
import org.naerbnic.wrapt.BasicValue

sealed trait WraptValue extends Any
case class BasicWraptValue(basicValue: BasicValue) extends AnyVal with WraptValue
case class ArrayValue(arrayValue: WraptArray) extends AnyVal with WraptValue
case class MapValue(mapValue: WraptMap) extends AnyVal with WraptValue