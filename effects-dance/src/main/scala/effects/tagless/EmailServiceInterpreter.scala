package effects
package tagless

import cats._
import cats.data._
import cats.implicits._


class EmailServiceInterpreter[M[_]](implicit me: MonadError[M, Throwable])
  extends EmailService[M] {

  def sendEmail(paymentCycle: PaymentCycle): M[Unit] = ().pure[M]
}

