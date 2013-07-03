package org.naerbnic.wrapt.primitive

import org.naerbnic.wrapt.Block

sealed trait WraptValue extends Any

object NullValue extends WraptValue
case class BoolValue(value: Boolean) extends AnyVal with WraptValue

case class IntValue(value: Long) extends AnyVal with WraptValue
case class FloatValue(value: Double) extends AnyVal with WraptValue

case class StringValue(stringValue: String) extends AnyVal with WraptValue
case class ArrayValue(arrayValue: WraptArray) extends AnyVal with WraptValue
case class MapValue(mapValue: WraptMap) extends AnyVal with WraptValue
case class BlobValue(blobValue: Block) extends AnyVal with WraptValue