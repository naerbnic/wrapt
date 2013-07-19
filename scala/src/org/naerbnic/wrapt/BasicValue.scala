package org.naerbnic.wrapt

import org.naerbnic.wrapt.util.Block

sealed trait BasicValue extends Any

object BasicValue {
  object NullValue extends BasicValue
  case class BoolValue(value: Boolean) extends AnyVal with BasicValue
  
  case class IntValue(value: Long) extends AnyVal with BasicValue
  case class FloatValue(value: Double) extends AnyVal with BasicValue
  
  case class StringValue(stringValue: String) extends AnyVal with BasicValue
  case class BlobValue(blobValue: Block) extends AnyVal with BasicValue
}