import Fibonacci.generate
import org.scalatest._

import scala.collection.immutable.List

class FibonacciSpec extends FlatSpec with Matchers {

  "Fibonacci" should "Return series for numbers from 1 to 10" in {
    generate(1, 10) should equal(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34))
  }

  "Fibonacci" should "Return series for numbers from 5 to 15" in {
    generate(5, 15) should equal(List(3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377))
  }
}
