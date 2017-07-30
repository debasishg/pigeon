package effects
package tagless

// algebra
trait EmailService[M[_]] {
  def sendEmail(paymentCycle: PaymentCycle): M[Unit]
}
