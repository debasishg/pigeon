package effects

import cats._
import cats.data._
import cats.implicits._

import Payments._

class PaymentServiceInterpreter[M[+_]](implicit me: MonadError[M, Throwable])
  extends PaymentService[M] with Utils {

  def paymentCycle: M[PaymentCycle] = PaymentCycle(10, 2014).pure[M]

  def qualifyingAccounts(p: PaymentCycle): M[Vector[Account]] =
    Vector.empty[Account].pure[M]

  def payments(accounts: Vector[Account]): M[Vector[Payment]] =
    Vector.empty[Payment].pure[M]

  def adjustTax(payments: Vector[Payment]): M[Vector[Payment]] =
    payments.pure[M]

  def postToLedger(payments: Vector[Payment]): M[PaymentProcessingResult] = {
      val amountToPost = valuation(payments)
      //.. do the posting
      PaymentProcessingResult.ProcessingSuccess(payments.map(_.account.emailAddress).toList).pure[M]
    }

  private def valuation(payments: Vector[Payment]): Money = {
    implicit val m: Monoid[Money] = Money.MoneyAddMonoid
    mapReduce(payments)(creditsOnly)
  }

}

