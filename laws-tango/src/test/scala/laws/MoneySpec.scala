package laws

import cats._
import kernel.laws.GroupLaws
import org.scalacheck.{ Arbitrary, Gen }
import Money._


class MoneySpec extends CatsSpec { def is = s2"""

  This is a specification for validating laws of Money

  (Money) should
     form a monoid under addition    $e1 
     form a monoid under ordering    $e2 
  """

  implicit lazy val arbCurrency: Arbitrary[Currency] = Arbitrary { Gen.oneOf(AUD, USD, INR, JPY) }

  implicit def moneyArbitrary: Arbitrary[Money] = 
    Arbitrary {
      for {
        i <- Arbitrary.arbitrary[Map[Currency, BigDecimal]]
      } yield new Money(i)
    }

  def e1 = checkAll("Money", GroupLaws[Money].monoid(MoneyAddMonoid))
  def e2 = checkAll("Money", GroupLaws[Money].monoid(MoneyOrderMonoid))
}

