package effects

import cats._
import cats.data._
import cats.implicits._

import Payments._

class PaymentServiceInterpreterWithKleisli[M[+_]](implicit me: MonadError[M, Throwable])
  extends PaymentServiceWithKleisli[M] with Utils {

  def paymentCycle: Kleisli[M, Config, PaymentCycle] =
    Kleisli((c: Config) => PaymentCycle(10, 2014).pure[M])

  def qualifyingAccounts: Kleisli[M, PaymentCycle, List[Account]] =
    Kleisli((p: PaymentCycle) => List.empty[Account].pure[M])

  def payments: Kleisli[M, List[Account], List[Payment]] =
    Kleisli((accounts: List[Account]) => List.empty[Payment].pure[M])

  def adjustTax: Kleisli[M, List[Payment], List[Payment]] =
    Kleisli((payments: List[Payment]) => payments.pure[M])

  def postToLedger: Kleisli[M, List[Payment], PaymentProcessingResult] =
    Kleisli {(payments: List[Payment]) => 
      val amountToPost = valuation(payments)
      //.. do the posting
      PaymentProcessingResult.ProcessingSuccess(payments.map(_.account.emailAddress)).pure[M]
    }

  private def valuation(payments: List[Payment]): Money = {
    implicit val m: Monoid[Money] = Money.MoneyAddMonoid
    mapReduce(payments)(creditsOnly)
  }

}