package laws

import org.specs2.Specification
import org.typelevel.discipline.specs2.Discipline
import cats.instances.AllInstances
import cats.syntax.AllSyntax

trait CatsSpec extends Specification with Discipline with AllInstances with AllSyntax
