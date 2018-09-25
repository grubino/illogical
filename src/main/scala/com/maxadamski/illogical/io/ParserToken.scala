package com.maxadamski.illogical.io

import com.maxadamski.illogical.data._

case class ParserToken(symbol: Symbol, value: String) {
  def matches(regex: String): Boolean = value.matches(regex)

  val notMatcher = raw"NOT|\~|\-|\!"
  val andMatcher = raw"AND|\&\&?"
  val orMatcher  = raw"OR|\|\|?"
  val existsMatcher = raw"E(X|XIST|XISTS)?"
  val forallMatcher = raw"(F|FOR)?A(LL)?"

  def isSep: Boolean = symbol == 'sep
  def isOp: Boolean = symbol == 'op
  def isId: Boolean = symbol == 'id

  def isLeftBrace: Boolean = isSep && matches(raw"[\(\[\{]")
  def isRightBrace: Boolean = isSep && matches(raw"[\)\]\}]")
  def isArgSep: Boolean = isSep && matches(raw"\,")

  def isEXISTS: Boolean = isOp && matches(existsMatcher)
  def isFORALL: Boolean = isOp && matches(forallMatcher)
  def isNOT: Boolean = isOp && matches(notMatcher)
  def isAND: Boolean = isOp && matches(andMatcher)
  def isOR: Boolean = isOp && matches(orMatcher)
  def isEQV: Boolean = isOp && matches(raw"EQV|IFF|\=\=\=|\<\>|\<\-\>")
  def isIMP: Boolean = isOp && matches(raw"IMP|THEN|\-\>|\>")
  def isNAND: Boolean = isOp && matches(s"(N|$notMatcher)($andMatcher)")
  def isNOR: Boolean = isOp && matches(s"(N|$notMatcher)($orMatcher)")
  def isXOR: Boolean = isOp && matches(s"X($orMatcher)")

  def isCon: Boolean = isId && matches(raw"^@[a-z]+[0-9]*'*")
  val isVar: Boolean = isId && matches(raw"^[a-z]+[0-9]*'*")
  val isFunc: Boolean = isId && matches(raw"^[a-z]+[0-9]*'*")
  val isPred: Boolean = isId && matches(raw"^[a-z]+[0-9]*'*")

  def isNOTEXISTS: Boolean = isOp && matches(s"($notMatcher)($existsMatcher)")
  def isNOTFORALL: Boolean = isOp && matches(s"($notMatcher)($forallMatcher)")

  def opToken: Option[OpToken] = 
    if (isAND) Some(AND)
    else if (isOR) Some(OR)
    else if (isNAND) Some(NAND)
    else if (isNOR) Some(NOR)
    else if (isXOR) Some(XOR)
    else if (isIMP) Some(IMP)
    else if (isEQV) Some(EQV)
    else None

  def quToken: Option[QuToken] =
    if (isFORALL) Some(FORALL)
    else if (isEXISTS) Some(EXISTS)
    else None

}

