package effects

import scala.concurrent._
import ExecutionContext.Implicits.global

import cats._
import cats.data._
import cats.implicits._

object Main {
  val paymentInterpreter = new PaymentServiceInterpreterWithKleisliAndErrorHandling[Future]
  val emailInterpreter = new EmailServiceInterpreter[Future]

  import paymentInterpreter._
  import emailInterpreter._

  processPayments andThen sendEmail

}

