package effects

import cats._
import cats.data._
import cats.implicits._


class EmailServiceInterpreter[M[+_]](implicit me: MonadError[M, Throwable])
  extends EmailService[M] {

  def sendEmail: Kleisli[M, PaymentProcessingResult, EmailSendResult] =
    Kleisli((result: PaymentProcessingResult) => result match {
      case PaymentProcessingResult.ProcessingSuccess(addresses) => EmailSendResult.EmailSent.pure[M]
      case PaymentProcessingResult.ProcessingFailure(th) => EmailSendResult.EmailSendFailure(th).pure[M]
    })
}

