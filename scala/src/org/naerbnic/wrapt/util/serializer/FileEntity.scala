package org.naerbnic.wrapt.util.serializer

sealed trait FileEntity

case class BlockEntity(block: BlockGenerator) extends FileEntity
case class MarkEntity(mark: Mark) extends FileEntity