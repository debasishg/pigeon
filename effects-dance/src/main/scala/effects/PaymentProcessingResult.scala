package effects

sealed trait PaymentProcessingResult
object PaymentProcessingResult {
  case object ProcessingSuccess extends PaymentProcessingResult
  case object ProcessingFailure extends PaymentProcessingResult
  case object DispatchFailure extends PaymentProcessingResult
  case object GeneralFailure extends PaymentProcessingResult
}


