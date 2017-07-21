package effects

import cats._
import cats.data._
import cats.implicits._

import Payments._

trait PaymentService[M[_]] {
  def processPayments(payments: List[Payment]): Kleisli[M, Config, PaymentProcessingResult]
}

trait Config

class PaymentServiceInterpreter[M[+_]](implicit me: Monad[M])
  extends PaymentService[M] with Utils {

  def processPayments(payments: List[Payment]): Kleisli[M, Config, PaymentProcessingResult] = 
    Kleisli((c: Config) => for {
      a <- adjustTax(valuation(payments))
      b <- postToLedger(a)
    } yield b)

  private def valuation(payments: List[Payment]): Money = {
    implicit val m: Monoid[Money] = Money.MoneyAddMonoid
    mapReduce(payments)(creditsOnly)
  }

  private def postToLedger(m: Money): M[PaymentProcessingResult] = PaymentProcessingResult.ProcessingSuccess.pure[M]
  private def adjustTax(amount: Money): M[Money] = amount.pure[M]
}

class PaymentServiceInterpreterWithErrorHandling[M[+_]](implicit me: MonadError[M, Throwable])
  extends PaymentService[M] with Utils {

  def processPayments(payments: List[Payment]): Kleisli[M, Config, PaymentProcessingResult] = 
    Kleisli((c: Config) => (for {
    a <- adjustTax(valuation(payments))
    b <- postToLedger(a)
  } yield b).recover { case _ => PaymentProcessingResult.ProcessingFailure })

  private def valuation(payments: List[Payment]): Money = {
    implicit val m: Monoid[Money] = Money.MoneyAddMonoid
    mapReduce(payments)(creditsOnly)
  }

  private def postToLedger(m: Money): M[PaymentProcessingResult] = PaymentProcessingResult.ProcessingSuccess.pure[M]
  private def adjustTax(amount: Money): M[Money] = amount.pure[M]
}
