package com.maxadamski.illogical.data

sealed abstract class QuToken {
  def isExistential: Boolean =
    this == EXISTS

  def isUniversal: Boolean =
    this == FORALL
}

sealed abstract class OpToken {
  def isCommutative: Boolean =
    List(OR, AND, NOR, NAND, XOR) contains this

  def isAssociative: Boolean =
    List(OR, AND, NOR, NAND, XOR) contains this
}

case object FORALL extends QuToken
case object EXISTS extends QuToken

case object OR extends OpToken
case object AND extends OpToken
case object NOR extends OpToken
case object NAND extends OpToken
case object XOR extends OpToken
case object IMP extends OpToken
case object EQV extends OpToken

