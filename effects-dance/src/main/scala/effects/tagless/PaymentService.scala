package effects
package tagless

import cats._
import cats.data._
import cats.implicits._

// algebra
trait PaymentService[M[_]] {
  def paymentCycle: M[PaymentCycle]
  def qualifyingAccounts(paymentCycle: PaymentCycle): M[Vector[Account]]
  def payments(accounts: Vector[Account]): M[Vector[Payment]]
  def adjustTax(payments: Vector[Payment]): M[Vector[Payment]]
  def postToLedger(payments: Vector[Payment]): M[Unit]

  def processPayments()(implicit me: Monad[M]) = for {
    p <- paymentCycle
    a <- qualifyingAccounts(p)
    m <- payments(a)
    a <- adjustTax(m)
    _ <- postToLedger(a)
  } yield p
}
