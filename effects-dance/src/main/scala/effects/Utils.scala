package effects

import cats._
import cats.data._
import cats.implicits._

trait Utils {
  def mapReduce[F[_], A, B](as: F[A])(f: A => B)
    (implicit fd: Foldable[F], m: Monoid[B]): B = fd.foldMap(as)(f)
}
