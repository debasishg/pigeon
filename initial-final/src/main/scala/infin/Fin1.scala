package infin

import cats._
import cats.data._
import cats.implicits._

// section 2.1 : final encoding
object Fin1 {
  type Repr = Int

  def lit: Int => Repr = identity
  def neg: Repr => Repr = r => -r
  def add: Repr => Repr => Repr = e1 => e2 => e1 + e2

  // evaluation for final encoding
  // Oleg says "The computation is compositional: the meaning of, for example, addition is computed
  // from the meaning, the value of, the summands. We see the first intimation
  // of the denotational semantics, with further to come."
  //
  val tf1 = add(lit(8))(neg(add(lit(1))(lit(2))))
}

// section 2.1: Oleg continues ..
//
// "In the final encoding, the evaluator is hardwired into the representation tf1, making it impossible 
// to interpret the object term as something other than integer. We want the final embedding to permit
// multiple interpretations too; we must therefore find a way to parameterize the ‘constructor functions’ 
// such as lit and neg by the result type."
// The right tool for this parameterization is the typeclass approach
trait ExpSYM[Repr] {

  // The constructor functions lit, neg and add have essentially the same signatures as before; the repr 
  // type however is now variable. The declaration of ExpSYM should remind us even more of the denotational 
  // semantics, over the semantic domain repr. As befits denotational semantics, the meaning of an expression, 
  // whatever repr happens to be, is computed from the meanings of the components
  def lit: Int => Repr
  def neg: Repr => Repr
  def add: Repr => Repr => Repr
}

trait ExpSYMVals {
  // To interpret finally-encoded expressions, we write an instance for ExpSYM, specifying the semantic domain. 
  // For example, we may interpret expressions as integers
  val intExpSYM = new ExpSYM[Int] {
    val lit = identity
    val neg = (r: Int) => -r
    val add = e1 => e2 => e1 + e2
  }

  val stringExpSYM = new ExpSYM[String] {
    val lit = _.show
    val neg = (r: String) => s"(-$r)"
    val add = e1 => e2 => s"($e1 + $e2)"
  }
}

object EvaluatorFin extends ExpSYMVals {
  def tf1[Repr]()(implicit sym: ExpSYM[Repr]) = {
    import sym._
    add(lit(8))(neg(add(lit(1))(lit(2))))
  }

  // evaluator of the final encoded term
  // Oleg says: 
  //
  // "The function eval has a strange type for an evaluator, and an even stranger definition – 
  // the identity function. It is more proper to call eval a selector of an interpretation as an integer. 
  // A finally-encoded expression has an indefinite number of interpretations; eval selects one of them."
  def eval: Int => Int = identity
  eval(tf1()(intExpSYM))

  // Multiple interpretations are now possible: we may interpret the very same term tf1 as a string, 
  // to pretty-print it.
  //
  // The pretty-printing interpreter is again the identity; indeed it does not do anything. Only its type matters, 
  // which selects from the multitude of, one may imagine, already computed interpretations.
  def view: String => String = identity
  view(tf1()(stringExpSYM))
}

// Diff between initial & final encoding :
//
// In the initial embedding, encoded object terms and their interpreters are ordinary, monomophic Scala values, 
// and hence are first-class. We may collect terms into a list, of the type List[Exp]: til1 = List(Lit 1, Add (Lit 1) (Lit 3))
// and interpret uniformly by mapping an evaluator, such as eval : til1 map eval gives the result List(1,4). 
// The final encoding represents object terms as polymorphic Scala values, which are not fully first-class: 
// storing them in data structures or passing as arguments generally loses polymorphism. In some cases, such as the 
// present one, it does not matter. We may still collect terms into a list tfl1 = [lit 1, add (lit 1) (lit 3)] and 
// then map F.eval tfl1 obtaining the same [1,4].
