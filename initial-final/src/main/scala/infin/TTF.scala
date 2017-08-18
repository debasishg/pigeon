package infin

import cats._
import cats.data._
import cats.implicits._

object TTF {
  trait Symantics[Repr[_]] {
    def int: Int => Repr[Int]
    def add: Repr[Int] => Repr[Int] => Repr[Int]

    def lam[A, B]: (Repr[A] => Repr[B]) => Repr[A => B]
    def app[A, B]: Repr[A => B] => Repr[A] => Repr[B]
  }

  def th1[Repr[_]](implicit ri: Symantics[Repr]): Repr[Int] = {
    import ri._
    add(int(1))(int(2))
  }
  
  def th2[Repr[_]](implicit ri: Symantics[Repr]): Repr[Int => Int] = {
    import ri._
    lam((x: Repr[Int]) => add(x)(x))
  }

  def th3[Repr[_]](implicit ri: Symantics[Repr]) = {
    import ri._
    lam((x: Repr[Int => Int]) => add(app(x)(int(1)))(int(2)))
  }

  object Evaluator {
    case class R[A](val unR: A) extends AnyVal 
  
    implicit def RSymantics = new Symantics[R] {
      def int: Int => R[Int] = R(_)
      def add: R[Int] => R[Int] => R[Int] = e1 => e2 => R(e1.unR + e2.unR)
      def lam[A, B]: (R[A] => R[B]) => R[A => B] = f => R(x => f(R(x)).unR)
      def app[A, B]: R[A => B] => R[A] => R[B] = rab => ra => R((rab.unR)(ra.unR))
    }
  
    def eval[T](e: R[T]): T = e.unR
    
    val th1_eval = eval(th1)
    // res0: Int = 3
  
    val th2_eval: Int => Int = eval(th2)
    val th2_eval1 = eval(th2)(21)
    // res2: Int = 42
  
    val th3_eval: (Int => Int) => Int = eval(th3)
    val th3_eval1 = eval(th3)(_ * 2)
    // res2: Int = 4
  }

  case class S[A](val unS: Int => String) extends AnyVal 

  implicit def SSymantics = new Symantics[S] {
    def int: Int => S[Int] = x => S((h: Int) => Function.const(x.show)(h))

    def add: S[Int] => S[Int] => S[Int] = e1 => e2 => S((h: Int) => s"( ${e1.unS(h)} + ${e2.unS(h)} )")

    def lam[A, B]: (S[A] => S[B]) => S[A => B] = f => S { h =>
      val x = s"x${h.show}"
      s"(\\\\$x -> ${f(S(Function.const(x)(_: Int))).unS(h + 1)})"
    }

    def app[A, B]: S[A => B] => S[A] => S[B] = sab => sa => S((h: Int) => s"(${sab.unS(h)} ${sa.unS(h)} )")
  }

  def view[T](s: S[T]): String = s.unS(0)

  val th1_view = view(th1)
  // res0: String = ( 1 + 2 )

  val th2_view = view(th2)
  // res1: String = (\\x0 -> ( x0 + x0 ))

  val th3_view = view(th3)
  // res2: String = (\\x0 -> ( (x0 1 ) + 2 ))
}
