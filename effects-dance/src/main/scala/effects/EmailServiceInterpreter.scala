package effects

import cats._
import cats.data._
import cats.implicits._


class EmailServiceInterpreterWithKleisli[M[_]](implicit me: Monad[M])
  extends EmailServiceWithKleisli[M] {

  def sendEmail: Kleisli[M, Unit, Unit] =
    Kleisli(_ => ().pure[M])
}

class EmailServiceInterpreter[M[_]](implicit me: MonadError[M, String])
  extends EmailService[M] {

  def sendEmail(paymentCycle: PaymentCycle): M[Unit] = ().pure[M]
}

