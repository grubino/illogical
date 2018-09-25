package com.maxadamski.illogical.data

case class Sub(v: Var, t: Term) {

  def substituting(sub: Set[Sub]): Sub =
    sub.find(_.v == v) getOrElse this

  def isFinal(s: Set[Sub]): Boolean =
    !s.exists(sub => t.contains(sub.v))

  override def toString: String = 
    s"${v.name} <- $t"

}

