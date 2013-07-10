package org.naerbnic.wrapt.serialize

sealed trait FileEntity

case class BlockEntity(block: BlockGenerator) extends FileEntity
case class MarkEntity(mark: Mark) extends FileEntity