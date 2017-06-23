package laws

import cats._
import kernel.laws.GroupLaws
import org.scalacheck.{ Arbitrary, Gen, Prop, Properties }
import Prop.forAll
import Arbitrary.arbitrary


class MoneySpec extends CatsSpec { def is = s2"""

  This is a specification for validating laws of Money

  (Money) should
     form a monoid under addition    $e1 
     form a monoid under ordering    $e2 
  """

  import MoneyDataGen._

  def e1 = checkAll("Money", GroupLaws[Money].monoid(Money.MoneyAddMonoid))
  def e2 = checkAll("Money", GroupLaws[Money].monoid(Money.MoneyOrderMonoid))

  /*
  implicit val MoneyMonoid: Monoid[Money] = Money.MoneyAddMonoid
  val x = for {
    m <- arbitrary[Money]
    n <- arbitrary[Money]
  } yield ((m.toBaseCurrency |+| n.toBaseCurrency), (m |+| n).toBaseCurrency)

  def e3 = Prop.forAll(x) { y => y._1.toBigInt == y._2.toBigInt }
  */
}

object MoneyDataGen {
  implicit lazy val arbCurrency: Arbitrary[Currency] = Arbitrary { Gen.oneOf(AUD, USD, INR, JPY) }

  implicit def moneyArbitrary: Arbitrary[Money] = Arbitrary {
    for {
      i <- Arbitrary.arbitrary[Map[Currency, BigDecimal]]
    } yield new Money(i)
  }
}
