package effects

import java.time.OffsetDateTime
import Money._

case class Account(no: String, name: String, openDate: OffsetDateTime, closeDate: Option[OffsetDateTime] = None)

case class Payment(account: Account, amount: Money, dateOfPayment: OffsetDateTime)

object Payments {
  def creditsOnly(p: Payment): Money = if (p.amount.isDebit) zeroMoney else p.amount
}
