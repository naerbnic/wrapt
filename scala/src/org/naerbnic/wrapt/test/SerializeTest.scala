package org.naerbnic.wrapt.test

import org.scalatest._
import org.naerbnic.wrapt.primitive.PrimFile
import scala.collection.immutable.SortedSet
import org.naerbnic.wrapt.primitive.PrimBasicValue
import org.naerbnic.wrapt.BasicValue.IntValue
import org.naerbnic.wrapt.serialize.PrimFileSerializer
import org.naerbnic.wrapt.BasicValue.FloatValue

class SerializeTest extends FunSpec {
  describe ("Serialization") {
    it ("can serialize a single int") {
      val file = new PrimFile() {
        override def indexes = SortedSet(0)
        override def getValue(index: Int) = index match {
          case 0 => Some(PrimBasicValue(IntValue(42)))
          case _ => None
        }
      }
      
      val entity = PrimFileSerializer.serialize(file)
      
      println(entity)
      
      val block = entity.toBlock()
      
      println(block.size)
      println(block.asByteBuffer().array().grouped(8).map(x => x.map(y => f"$y%02x").mkString)
          .mkString(" "))
    }
    
    it ("can serialize a single float") {
      val file = new PrimFile() {
        override def indexes = SortedSet(0)
        override def getValue(index: Int) = index match {
          case 0 => Some(PrimBasicValue(FloatValue(1.21d)))
          case _ => None
        }
      }
      
      val entity = PrimFileSerializer.serialize(file)
      
      println(entity)
      
      val block = entity.toBlock()
      
      println(block.size)
      println(block.asByteBuffer().array().grouped(8).map(x => x.map(y => f"$y%02x").mkString)
          .mkString(" "))
    }
    
    it ("can serialize multiple values") {
      val file = new PrimFile() {
        override def indexes = SortedSet(0, 1)
        override def getValue(index: Int) = index match {
          case 0 => Some(PrimBasicValue(FloatValue(1.21d)))
          case 1 => Some(PrimBasicValue(IntValue(12)))
          case _ => None
        }
      }
      
      val entity = PrimFileSerializer.serialize(file)
      
      println(entity)
      
      val block = entity.toBlock()
      
      println(block.size)
      println(block.asByteBuffer().array().grouped(8).map(x => x.map(y => f"$y%02x").mkString)
          .mkString(" "))
    }
  }
}