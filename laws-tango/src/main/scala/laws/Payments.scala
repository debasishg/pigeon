package laws

import java.time.OffsetDateTime
import Money._

import cats._
import cats.data._
import cats.implicits._

object Payments extends Utils {
  case class Account(no: String, name: String, openDate: OffsetDateTime, closeDate: Option[OffsetDateTime] = None)
  case class Payment(account: Account, amount: Money, dateOfPayment: OffsetDateTime)

  def creditsOnly(p: Payment): Money = if (p.amount.isDebit) zeroMoney else p.amount

  // concrete implementation
  def valuationConcrete(payments: List[Payment]) = payments.foldLeft(zeroMoney) { (a, e) =>
    add(a, creditsOnly(e))
  }

  // generic implementation
  def valuation(payments: List[Payment])(implicit m: Monoid[Money]) =
    mapReduce(payments)(creditsOnly)
}
