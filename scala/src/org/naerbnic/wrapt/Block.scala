package org.naerbnic.wrapt

import java.nio.ByteBuffer

trait Block {
  def size: Long
  def asByteBuffer: ByteBuffer
}