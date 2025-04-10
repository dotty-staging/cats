/*
 * Copyright (c) 2015 Typelevel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package cats
package kernel
package laws
package discipline

import cats.kernel.instances.option._
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

trait SemigroupTests[A] extends Laws {
  def laws: SemigroupLaws[A]

  def semigroup(implicit arbA: Arbitrary[A], eqA: Eq[A]): RuleSet =
    new DefaultRuleSet(
      "semigroup",
      None,
      "associative" -> forAll(laws.semigroupAssociative),
      "repeat1" -> forAll(laws.repeat1),
      "repeat2" -> forAll(laws.repeat2),
      "combineAllOption" -> forAll(laws.combineAllOption),
      "reverseReverses" -> forAll(laws.reverseReverses),
      "reverseRepeat1" -> forAll(laws.reverseRepeat1),
      "reverseRepeat2" -> forAll(laws.reverseRepeat2),
      "reverseCombineAllOption" -> forAll(laws.reverseCombineAllOption),
      "intercalateIntercalates" -> forAll(laws.intercalateIntercalates),
      "intercalateRepeat1" -> forAll(laws.intercalateRepeat1),
      "intercalateRepeat2" -> forAll(laws.intercalateRepeat2),
      "intercalateCombineAllOption" -> forAll(laws.intercalateCombineAllOption)
    )
}

object SemigroupTests {
  def apply[A: Semigroup]: SemigroupTests[A] =
    new SemigroupTests[A] { def laws: SemigroupLaws[A] = SemigroupLaws[A] }
}
