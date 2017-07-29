package effects

import cats._
import cats.data._
import cats.implicits._


// algebra
trait EmailService[M[_]] {
  def sendEmail(paymentCycle: PaymentCycle): M[Unit]
}

// algebra
trait EmailServiceWithKleisli[M[_]] {
  def sendEmail: Kleisli[M, Unit, Unit]
}
