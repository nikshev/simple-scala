import Tree.{list2btree, nodeElementsSum, sizeTree, treeHeight}
import org.scalatest._

class BraceSpec extends FlatSpec with Matchers {

  "Braces check string method case 1" should " be successful" in {
    val str="{}"
    val rules=Map('{'->'}')
    Braces.checkString(str,rules) should be (true)
  }

  "Braces check string method case 2" should " be successful" in {
    val str="{}{"
    val rules=Map('{'->'}')
    Braces.checkString(str,rules) should be (false)
  }

  "Braces check string method case 3" should " be successful" in {
    val str="{}{{}}"
    val rules=Map('{'->'}')
    Braces.checkString(str,rules) should be (true)
  }

  "Braces check string method case 4" should " be successful" in {
    val str="{}{{}}[]"
    val rules=Map('{'->'}','['->']')
    Braces.checkString(str,rules) should be (true)
  }
}
