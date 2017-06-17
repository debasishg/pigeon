package laws

import cats._
import cats.data._
import cats.implicits._

sealed trait Currency
case object USD extends Currency
case object AUD extends Currency
case object JPY extends Currency
case object INR extends Currency

class Money private[laws] (val items: Map[Currency, BigDecimal]) {
  def toBaseCurrency: BigDecimal = items.foldLeft(BigDecimal(0)) { (a, i) =>
    val ccy = i._1
    val amount = i._2
    a + Money.exchangeRateWithUSD.get(ccy).getOrElse(BigDecimal(1)) * amount
  }
}

object Money {
  def apply(amount: BigDecimal, ccy: Currency) = new Money(Map(ccy -> amount))
  def add(m: Money, amount: BigDecimal, ccy: Currency) = new Money(m.items |+| Map(ccy -> amount))
  def add(m: Money, n: Money) = new Money(m.items |+| n.items)

  final val exchangeRateWithUSD: Map[Currency, BigDecimal] = 
    Map(AUD -> 0.76, JPY -> 0.009, INR -> 0.016, USD -> 1.0)

  implicit val MoneyAddMonoid: Monoid[Money] = new Monoid[Money] {
    def combine(m: Money, n: Money): Money = add(m, n)
    def empty: Money = new Money(Map.empty[Currency, BigDecimal])
  }

  implicit val MoneyEq: Eq[Money] = new Eq[Money] {
    def eqv(m: Money, n: Money): Boolean = m.items === n.items
  }

  implicit val MoneyOrderMonoid: Order[Money] = new Order[Money] {
    def compare(m: Money, n: Money) = {
      val mbase = m.toBaseCurrency
      val nbase = n.toBaseCurrency
      if (mbase > nbase) 1
      else if (mbase < nbase) -1
      else 0
    }
  }
}
