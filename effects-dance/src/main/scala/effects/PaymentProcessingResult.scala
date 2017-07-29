package effects

sealed trait PaymentProcessingResult
object PaymentProcessingResult {
  case class ProcessingSuccess(emailAddresses: List[EmailAddress]) extends PaymentProcessingResult
  case class ProcessingFailure(reason: String) extends PaymentProcessingResult
}

sealed trait EmailSendResult
object EmailSendResult {
  case object EmailSent extends EmailSendResult
  case class EmailSendFailure(reason: String) extends EmailSendResult
}

