package effects

import cats._
import cats.data._
import cats.implicits._

import Payments._

class PaymentServiceInterpreterWithErrorHandling[M[+_]](implicit me: MonadError[M, Throwable])
  extends PaymentServiceWithErrorHandling[M] with Utils {

  def paymentCycle: M[PaymentCycle] = PaymentCycle(10, 2014).pure[M]

  def qualifyingAccounts: PaymentCycle => M[List[Account]] =
    (p: PaymentCycle) => List.empty[Account].pure[M]

  def payments: List[Account] => M[List[Payment]] =
    (accounts: List[Account]) => List.empty[Payment].pure[M]

  def adjustTax: List[Payment] => M[List[Payment]] =
    (payments: List[Payment]) => payments.pure[M]

  def postToLedger: List[Payment] => M[PaymentProcessingResult] =
    (payments: List[Payment]) => { 
      val amountToPost = valuation(payments)
      //.. do the posting
      PaymentProcessingResult.ProcessingSuccess.pure[M]
    }

  private def valuation(payments: List[Payment]): Money = {
    implicit val m: Monoid[Money] = Money.MoneyAddMonoid
    mapReduce(payments)(creditsOnly)
  }

}

