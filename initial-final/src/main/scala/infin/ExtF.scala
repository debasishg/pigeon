package infin

import cats._
import cats.data._
import cats.implicits._

// Tagless Typed Interpreters: extensibility

object ExtF {
  import infin.{Intro2 => F}
  import F._

  // We extend the final representation of the language with a new
  // expression form: multiplication

  trait MulSYM[Repr] {
    def mul: Repr => Repr => Repr
  }

  def tfm1[Repr]()(implicit sym: ExpSYM[Repr], msym: MulSYM[Repr]) = {
    import sym._
    import msym._
    add(lit(7))(neg(mul(lit(1))(lit(2))))
  }

  // We can even use a previously defined unextended expression (tf1)
  // as a part of the extended expression.
  // We can indeed mix-and-match
  def tfm2[Repr]()(implicit sym: ExpSYM[Repr], msym: MulSYM[Repr]) = {
    import sym._
    import msym._
    mul(lit(7))(tf1)
  }
  
  // We extend the specific interpreters thusly
  
  val intMulSYM = new MulSYM[Int] {
    val mul = e1 => e2 => e1 * e2
  }

  // The definition of eval stays the same. Why ?
  // The extension automatically kicks in
  val tfm1_eval = eval(tfm1()(intExpSYM, intMulSYM))
  // 5
  val tfm2_eval = eval(tfm2()(intExpSYM, intMulSYM))
  // 35

  // We extend the view interpreter just as well

  val stringMulSYM = new MulSYM[String] {
    val mul = e1 => e2 => s"($e1 * $e2)"
  }

  val tfm1_view = view(tfm1()(stringExpSYM, stringMulSYM))
  // res0: String = (7 + (-(1 * 2)))

  val tfm2_view = view(tfm2()(stringExpSYM, stringMulSYM))
  // res1: String = (7 * (8 + (-(1 + 2))))

  def tfl1[Repr: ExpSYM] = List(F.tf1)
  def tfl2[Repr :ExpSYM :MulSYM]: List[Repr] = List(tfm1, tfm2) ::: tfl1

  val tfl2_eval = tfl2(intExpSYM, intMulSYM) map eval
  // res0: List[Int] = List(5, 35, 5

  val tfl2_view = tfl2(stringExpSYM, stringMulSYM) map view
  // res0: List[String] = List((7 + (-(1 * 2))), (7 * (8 + (-(1 + 2)))), (8 + (-(1 + 2))))
}

