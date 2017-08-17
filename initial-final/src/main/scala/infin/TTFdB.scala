package infin

object TTFdB {

  trait Symantics[Repr[_, _]] {
    def int[H]: Int => Repr[H, Int]
    def add[H]: Repr[H, Int] => Repr[H, Int] => Repr[H, Int]

    def z[H, A]: Repr[(A, H), A]
    def s[H, A, B]: Repr[H, A] => Repr[(B, H), A]
    def lam[H, A, B]: Repr[(A, H), B] => Repr[H, A => B]
    def app[H, A, B]: Repr[H, A => B] => Repr[H, A] => Repr[H, B]
  }

  def td1[H, Repr[_, _]](implicit is: Symantics[Repr]): Repr[H, Int] = is.add(is.int(1))(is.int(2))

  def td2[H, Repr[_, _]](implicit is: Symantics[Repr]): Repr[H, Int => Int] = {
    import is._
    lam(add(z[H, Int])(z[H, Int]))
  }

  def td2o[H, Repr[_, _]](implicit is: Symantics[Repr]): Repr[(Int, H), (Int => Int)] = {
    import is._
    lam(add(z[(Int, H), Int])(s[(Int, H), Int, Int](z[H, Int])))
  }

  def td3[H, Repr[_, _]](implicit is: Symantics[Repr]): Repr[H, (Int => Int) => Int] = {
    import is._
    lam(
      add
        (app[(Int => Int, H), Int, Int]
          (z[H, Int => Int])
          (int[(Int => Int, H)](1))
        )
        (int[(Int => Int, H)](2))
    )
  }
}





/*
// Lambda calculus represented by `R`, using HOAS
abstract class Symantics[R[_]] {
  def int(n: Int): R[Int]
  def bool(b: Boolean): R[Boolean]

  def lam[A,B](f: R[A] => R[B]): R[A => B]
  def app[A,B](f: R[A => B], a: R[A]): R[B]
  def fix[A](f: R[A] => R[A]): R[A]

  def add(a: R[Int], b: R[Int]): R[Int]
  def mul(a: R[Int], b: R[Int]): R[Int]
  def leq(a: R[Int], b: R[Int]): R[Boolean]
  def if_[A](b: R[Boolean], t: () => R[A], f: () => R[A]): R[A]
}
*/
