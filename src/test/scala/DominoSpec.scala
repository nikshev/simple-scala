import Domino.solution
import scala.collection.immutable.List
import org.scalatest._

class DominoSpec extends FlatSpec with Matchers {

  "Domino" should "Return chain 56:62:21:13:36 for numbers (1,2),(1,3),(2,6),(3,6),(5,6)" in {
    solution(List(List(1,2),List(1,3),List(2,6),List(3,6),List(5,6))) should equal("56:63:31:12:26")
    solution(List(List(1,2),List(1,3),List(2,6),List(3,6),List(4,7))) should equal("63:31:12:26")
  }

}
