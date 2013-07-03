package org.naerbnic.wrapt

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.channels.FileChannel.MapMode

trait Block {
  /**
   * The number of bytes in this block, in bytes
   */
  def size: Long
  
  /**
   * Returns a byte buffer into this block. It may be a direct buffer, or a
   * copy depending on this size of the buffer, and the underlying type.
   */
  def getByteBuffer(offset: Long, size: Int): ByteBuffer
  
  /**
   * Reads data from the block into the given buffer at the given offset.
   * The amount read is dst.remaining().
   */
  def read(dst: ByteBuffer, offset: Long)
  
  /**
   * Returns a byte buffer for this entire block. This requires that the
   * size of the block is less than 2GB
   */
  def asByteBuffer() = {
    require (size <= Int.MaxValue)
    getByteBuffer(0, size.toInt)
  }
  
  def readInt(offset: Long) = {
    val sizeBuffer = ByteBuffer.allocate(4)
    read(sizeBuffer, offset)
    sizeBuffer.asIntBuffer().get(0)
  }
  
  def readLong(offset: Long) = {
    val sizeBuffer = ByteBuffer.allocate(8)
    read(sizeBuffer, offset)
    sizeBuffer.asLongBuffer().get(0)
  }
  
  def readByte(offset: Long) = {
    val sizeBuffer = ByteBuffer.allocate(1)
    read(sizeBuffer, offset)
    sizeBuffer.get(0)
  }
  
  /**
   * Returns a block that is a part of this block. This is generally more
   * lightweight than getting it as a byte buffer, as the block may be to a
   * structure that is not in memory.
   */
  def getSubBlock(offset: Long, size: Long): Block = {
    require (size >= 0)
    require (offset + size <= this.size)
    require (offset >= 0)
    
    new Block.SubBlock(this, offset, size)
  }
  
  /**
   * Same as getSubBlock above, but takes the sub block starting at offset,
   * and containing the remainder of this block.
   */
  def getSubBlock(offset: Long): Block = getSubBlock(offset, size - offset)
  
  /**
   * Returns a block with the same constants as this one. The contents are
   * guaranteed to be in memory.
   */
  def reify(): Block = new Block.ByteBufferBlock(asByteBuffer())
}

object Block {
  private class FileBlock(channel: FileChannel)
      extends Block {
    override def size = channel.size()
    override def getByteBuffer(offset: Long, size: Int) = {
      require (size >= 0)
      channel.map(MapMode.READ_ONLY, offset, size)
    }
      
    override def read(dst: ByteBuffer, offset: Long) = {
      require (offset + dst.remaining() <= size)
      require (offset >= 0)
      
      channel.read(dst, offset)
    }
  }
  
  def fromFile(channel: FileChannel): Block = new FileBlock(channel)
  
  private class SubBlock(subblock: Block, offset: Long, val size: Long) extends Block {
    override def getByteBuffer(offset: Long, size: Int) = {
      require (size >= 0)
      require (offset >= 0)
      require (offset + size <= this.size)
      
      subblock.getByteBuffer(this.offset + offset, size)
    }
    
    override def read(dst: ByteBuffer, offset: Long) = {
      require (offset >= 0)
      require (offset + dst.remaining() <= size)
      subblock.read(dst, offset + this.offset)
    }
    
    override def getSubBlock(offset: Long, size: Long): Block = {
      require (size >= 0)
      require (offset >= 0)
      require (offset + size <= this.size)
      
      new Block.SubBlock(subblock, this.offset + offset, size)
    }
  }
  
  private class ByteBufferBlock(buffer: ByteBuffer) extends Block {
    override def size = buffer.capacity()
    override def getByteBuffer(offset: Long, size: Int) = {
      require (offset >= 0)
      require (size >= 0)
      require (offset + size <= buffer.capacity())
      
      buffer.position(offset.toInt)
      buffer.limit(offset.toInt + size)
      buffer.slice()
    }
    
    override def read(dst: ByteBuffer, offset: Long) = {
      require (offset >= 0)
      require (offset + dst.remaining() <= size)
      buffer.position(offset.toInt)
      buffer.limit(offset.toInt + dst.remaining())
      dst.put(buffer)
    }
    
    override def reify() = this
  }
}