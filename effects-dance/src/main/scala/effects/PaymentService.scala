package effects

import cats._
import cats.data._
import cats.implicits._

// algebra
trait PaymentService[M[_]] {
  def paymentCycle: M[PaymentCycle]
  def qualifyingAccounts(paymentCycle: PaymentCycle): M[Vector[Account]]
  def payments(accounts: Vector[Account]): M[Vector[Payment]]
  def adjustTax(payments: Vector[Payment]): M[Vector[Payment]]
  def postToLedger(payments: Vector[Payment]): M[PaymentProcessingResult]

  def processPayments()(implicit me: MonadError[M, Throwable]) = (for {
    p <- paymentCycle
    a <- qualifyingAccounts(p)
    m <- payments(a)
    a <- adjustTax(m)
    l <- postToLedger(a)
  } yield l).recover { 
    case th: Throwable => PaymentProcessingResult.ProcessingFailure(th) 
  }
}

// algebra
trait PaymentServiceWithKleisli[M[_]] {
  def paymentCycle: Kleisli[M, Config, PaymentCycle]
  def qualifyingAccounts: Kleisli[M, PaymentCycle, List[Account]]
  def payments: Kleisli[M, List[Account], List[Payment]]
  def adjustTax: Kleisli[M, List[Payment], List[Payment]]
  def postToLedger: Kleisli[M, List[Payment], PaymentProcessingResult]

  def processPayments()(implicit me: MonadError[M, Throwable]) = 
    (paymentCycle         andThen
     qualifyingAccounts   andThen
     payments             andThen
     adjustTax            andThen
     postToLedger).recover { 
       case th: Throwable => PaymentProcessingResult.ProcessingFailure(th) 
     }
}
