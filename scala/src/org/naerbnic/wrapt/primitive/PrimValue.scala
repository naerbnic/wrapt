package org.naerbnic.wrapt.primitive

import org.naerbnic.wrapt.BasicValue

sealed trait PrimValue extends Any
case class PrimBasicValue(basicValue: BasicValue) extends AnyVal with PrimValue
case class PrimArrayValue(arrayValue: PrimArray) extends AnyVal with PrimValue
case class PrimMapValue(mapValue: PrimMap) extends AnyVal with PrimValue