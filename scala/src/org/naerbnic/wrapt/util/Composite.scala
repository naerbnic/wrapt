package org.naerbnic.wrapt.util

trait Composite[T] {
  def asSeq: Seq[T]
}

object Composite {
  private class Singleton[T](item: T) extends Composite[T] {
    def asSeq = Seq(item)
  }
  
  private class SequenceComposite[T] (
      children: Seq[Composite[T]]) extends Composite[T] {
    def asSeq = Seq.concat(children.map(_.asSeq): _*)
  }
  
  def apply[T](items: T*): Composite[T] = {
    if (items.size == 0) {
      new SequenceComposite(Seq[Composite[T]]())
    } else if (items.size == 1) {
      new Singleton(items(0))
    } else {
      new SequenceComposite(Seq(items.map(new Singleton(_)): _*))
    }
  }
  
  def concat[T](items: Composite[T]*): Composite[T] = {
    new SequenceComposite(Seq(items: _*))
  }
}