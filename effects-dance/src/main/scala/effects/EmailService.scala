package effects

import cats._
import cats.data._
import cats.implicits._


// algebra
trait EmailService[M[_]] {
  def sendEmail: Kleisli[M, PaymentProcessingResult, EmailSendResult]
}
