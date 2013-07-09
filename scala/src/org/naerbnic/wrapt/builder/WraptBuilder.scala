package org.naerbnic.wrapt.builder

import org.naerbnic.wrapt.Block
import org.naerbnic.wrapt.BasicValue
import org.naerbnic.wrapt.BasicValue

trait WraptBuilder {
  type Handle
  
  trait MapBuilder {
    def handle: Handle
    def insert(key: String, value: Handle)
  }
  
  trait ArrayBuilder {
    def handle: Handle 
    def insert(value: Handle)
  }
  
  def createInt(i: Long): Handle
  def createFloat(f: Double): Handle
  def createBlob(b: Block): Handle
  def createNull(): Handle
  def createBool(b: Boolean): Handle
  def createMap(t: String): MapBuilder
  def createArray(): ArrayBuilder
}