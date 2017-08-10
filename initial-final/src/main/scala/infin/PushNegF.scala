package infin

import cats._
import cats.data._
import cats.implicits._

object PushNegF {

  import infin.Intro2._

  // section 2.4: Pushing negation down: the final view
  // The operation of pushing negation down is indeed non-compositional, because the processing of a negated expression 
  // depends on its context. To be precise, it depends on whether the negated expression appears as part of a negated
  // expression. We make that context-dependence explicit:
  sealed trait Ctx
  case object Pos extends Ctx
  case object Neg extends Ctx

  def ctxExpSYM[Repr: ExpSYM] = new ExpSYM[Ctx => Repr] {

    val r = implicitly[ExpSYM[Repr]]
    
    def lit: Int => Ctx => Repr = { n => c =>
      c match {
        case Pos => r.lit(n)
        case Neg => r.neg(r.lit(n))
      }
    }
    def neg: (Ctx => Repr) => Ctx => Repr = { e => c =>
      c match {
        case Pos => e(Neg)
        case Neg => e(Pos)
      }
    }
    def add: (Ctx => Repr) => (Ctx => Repr) => Ctx => Repr = { e1 => e2 => c =>
      r.add(e1(c))(e2(c)) // homomorphism
    }
  }

  // The interpreter for pushing the negation down
  def push_neg[Repr]: (Ctx => Repr) => Repr = _(Pos)

  // val tf1_view = view(tf1()(stringExpSYM))
}

