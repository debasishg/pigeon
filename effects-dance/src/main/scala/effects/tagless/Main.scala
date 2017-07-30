package effects
package tagless

import cats._
import cats.data._
import cats.implicits._

// monix task based computation
object MonixTaskModule {
  import monix.eval.Task
  import monix.cats._

  val paymentInterpreter = new PaymentServiceInterpreter[Task]
  val emailInterpreter = new EmailServiceInterpreter[Task]

  for {
    p <- paymentInterpreter.processPayments
    e <- emailInterpreter.sendEmail(p)
  } yield e

}

// future based computation
object FutureModule {
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global
  
  val paymentInterpreter = new PaymentServiceInterpreter[Future]
  val emailInterpreter = new EmailServiceInterpreter[Future]

  for {
    p <- paymentInterpreter.processPayments
    e <- emailInterpreter.sendEmail(p)
  } yield e

}

// try task based computation
object TryModule {
  import scala.util.Try

  val paymentInterpreter = new PaymentServiceInterpreter[Try]
  val emailInterpreter = new EmailServiceInterpreter[Try]

  for {
    p <- paymentInterpreter.processPayments
    e <- emailInterpreter.sendEmail(p)
  } yield e

}

// stackable
object VerticalComposition {
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global
  
  val auditablePaymentInterpreter = new AuditablePaymentService[Future](
    new PaymentServiceInterpreter[Future])

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
