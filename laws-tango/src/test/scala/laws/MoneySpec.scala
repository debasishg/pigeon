package laws

import cats._
import kernel.laws.GroupLaws
import org.scalacheck.{ Arbitrary, Gen }


class MoneySpec extends CatsSpec { def is = s2"""
  (Money, +) should
     form a monoid    $e1 
  """

  implicit lazy val arbCurrency: Arbitrary[Currency] = Arbitrary { Gen.oneOf(AUD, USD, INR, JPY) }

  implicit def moneyArbitrary: Arbitrary[Money] = 
    Arbitrary {
      for {
        i <- Arbitrary.arbitrary[Map[Currency, BigDecimal]]
      } yield new Money(i)
    }

  def e1 = checkAll("Money", GroupLaws[Money].monoid(Monoid[Money]))
}
