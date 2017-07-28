package effects

import cats._
import cats.data._
import cats.implicits._

import Payments._

final class AuditablePaymentService[M[_]: Applicative](paymentService: PaymentService[M]) 
  extends PaymentService[WriterT[M, Vector[String], ?]] {

  def paymentCycle: WriterT[M, Vector[String], PaymentCycle] =
    WriterT.lift(paymentService.paymentCycle)

  def qualifyingAccounts(paymentCycle: PaymentCycle): WriterT[M, Vector[String], Vector[Account]] =
    WriterT.lift(paymentService.qualifyingAccounts(paymentCycle))

  def payments(accounts: Vector[Account]): WriterT[M, Vector[String], Vector[Payment]] =
    WriterT.putT(paymentService.payments(accounts))(accounts.map(_.no))

  def adjustTax(payments: Vector[Payment]): WriterT[M, Vector[String], Vector[Payment]] =
    WriterT.putT(paymentService.adjustTax(payments))(payments.map(_.toString))

  def postToLedger(payments: Vector[Payment]): WriterT[M, Vector[String], PaymentProcessingResult] =
    WriterT.lift(paymentService.postToLedger(payments))
}
