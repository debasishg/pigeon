package infin

import cats._
import cats.data._
import cats.implicits._

// section 2.1 : Initial encoding
trait Exp
case class Lit(i: Int) extends Exp
case class Neg(e: Exp) extends Exp
case class Add(l: Exp, r: Exp) extends Exp

object Exp {
  // Interpreters that we saw below are compositional. But not all operations are compositional.
  // There are however many operations – for example, program transformations and optimizations – that do not seem 
  // compositional because the handling of a sub-expression does depend on where it appears in a larger expression (i.e.,
  // depends on the context).
  //
  // section 2.4: Pushing negation down : the initial view (making seemingly non compositional operations compose)
  // Oleg says:
  //
  // "The type of push_neg emphasizes that we are transforming one expression to another; the result is an embedded 
  // expression in its own right and can be processed with any existing interpreter. The transformed expression 
  // should be equivalent to the source with respect to a set of laws"
  def push_neg: Exp => Exp = { exp: Exp =>
    exp match {
      case e@Lit(_) => e
      case e@(Neg(Lit(_))) => e
      case Neg(Neg(e)) => e
      case Neg(Add(e1, e2)) => Add(push_neg(Neg(e1)), push_neg(Neg(e2)))
      case Add(e1, e2) => Add(push_neg(e1), push_neg(e2))
    }
  }
}

object Evaluator {
  // All interpreters of the langauge are compositional - basically a fold realized through pattern matching
  // and general recursion in the initial approach. Note difference in the final approach.
  //
  // interpreter 1 : evaluator for the initial encoding
  def eval: Exp => Int = { e: Exp =>
    e match {
      case Lit(i) => i
      case Neg(exp) => - eval(exp)
      case Add(e1, e2) => eval(e1) + eval(e2)
    }
  }

  // interpreter 2 : view for the initial encoding
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

  import Exp._
  val ti1_norm = push_neg(ti1)
  val ti1_norm_view = view(ti1_norm)
  val ti1_norm_eval = eval(ti1_norm)

  /**
  scala> import infin._
  import infin._
  
  scala> import Evaluator._
  import Evaluator._
  
  scala> ti1
  res0: infin.Add = Add(Lit(8),Neg(Add(Lit(1),Lit(2))))
  
  scala> eval(ti1)
  res1: Int = 5
  
  scala> view(ti1)
  res2: String = (8 + (-(1 + 2)))
  
  scala> ti1_norm
  res3: infin.Exp = Add(Lit(8),Add(Neg(Lit(1)),Neg(Lit(2))))
  
  scala> ti1_norm_view
  res4: String = (8 + ((-1) + (-2)))
  
  scala> ti1_norm_eval
  res5: Int = 5
  **/

  val til1: List[Exp] = List(Lit(1), Add(Lit(1), Lit(3)))
  til1 map eval
}
