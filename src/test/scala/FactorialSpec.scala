import Factorial._
import org.scalatest._

import scala.collection.immutable.List

class FactorialSpec extends FlatSpec with Matchers {

  "Factorial 5" should "return 120" in {
    factorial(5) should equal(120)
  }

  "Factorial 4" should "return 24" in {
    factorial(4) should equal(24)
  }

  "Factorial 3" should "return 6" in {
    factorial(3) should equal(6)
  }

  "Factorial 2" should "return 2" in {
    factorial(2) should equal(2)
  }
}
