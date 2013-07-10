package org.naerbnic.wrapt.util

object FuncUtil {
  def merge[A, B] (f1: A => B, f2: A => B) (mergef: (B, B) => B) = { v: A =>
    mergef(f1(v), f2(v))
  }
}