package effects

sealed trait PaymentProcessingResult
object PaymentProcessingResult {
  case class ProcessingSuccess(emailAddresses: List[EmailAddress]) extends PaymentProcessingResult
  case class ProcessingFailure(reason: Throwable) extends PaymentProcessingResult
}

sealed trait EmailSendResult
object EmailSendResult {
  case object EmailSent extends EmailSendResult
  case class EmailSendFailure(reason: Throwable) extends EmailSendResult
}

