package infin

import cats._
import cats.data._
import cats.implicits._

// section 2.1 : Initial encoding
trait Exp
case class Lit(i: Int) extends Exp
case class Neg(e: Exp) extends Exp
case class Add(l: Exp, r: Exp) extends Exp

object Evaluator {
  // evaluator for the initial encoding
  def eval: Exp => Int = { e: Exp =>
    e match {
      case Lit(i) => i
      case Neg(exp) => - eval(exp)
      case Add(e1, e2) => eval(e1) + eval(e2)
    }
  }

  // another evaluator (view) for the initial encoding
  def view: Exp => String = { e: Exp =>
    e match {
      case Lit(i) => i.show
      case Neg(exp) => s"(-${view(exp)})"
      case Add(e1, e2) => s"(${view(e1)} + ${view(e2)})"
    }
  }

  val ti1 = Add(Lit(8), (Neg(Add(Lit(1), Lit(2)))))
  eval(ti1)
  view(ti1)

  val til1: List[Exp] = List(Lit(1), Add(Lit(1), Lit(3)))
  til1 map eval
}
