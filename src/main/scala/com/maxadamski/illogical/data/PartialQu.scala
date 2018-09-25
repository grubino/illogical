package com.maxadamski.illogical.data

case class PartialQu(token: QuToken, variable: Var) {

  def complete(form: Form): Form = 
    Qu(token, variable, form)

  def isExistential: Boolean =
    token.isExistential

  def isUniversal: Boolean =
    token.isUniversal

}

