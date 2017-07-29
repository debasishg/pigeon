package effects

import cats._
import cats.data._
import cats.implicits._

object Main {
  val auditablePaymentInterpreter = new AuditablePaymentService[Either[String, ?]](
    new PaymentServiceInterpreter[Either[String, ?]])

  // either based computation
  val paymentInterpreter = new PaymentServiceInterpreter[Either[String, ?]]
  val emailInterpreter = new EmailServiceInterpreter[Either[String, ?]]

  for {
    p <- paymentInterpreter.processPayments
    e <- emailInterpreter.sendEmail(p)
  } yield e

  // vertical composition
  import auditablePaymentInterpreter._

  def processPayments() = for {
    p <- paymentCycle
    a <- qualifyingAccounts(p)
    m <- payments(a)
    a <- adjustTax(m)
    _ <- postToLedger(a)
  } yield ()

}

object KliesliModule {
  val paymentInterpreter = new PaymentServiceInterpreterWithKleisli[Either[String, ?]]
  val emailInterpreter = new EmailServiceInterpreterWithKleisli[Either[String, ?]]

  import paymentInterpreter._
  import emailInterpreter._

  processPayments andThen sendEmail
}
