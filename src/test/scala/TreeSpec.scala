import Tree.{list2btree, nodeElementsSum, sizeTree, treeHeight}
import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {

  "Tree" should "AList(6,2,3,1,4) must return correct answers" in {
    val tree=list2btree(List(6,2,3,1,4))
    sizeTree(tree) should equal(5)
    nodeElementsSum(tree) should equal(16)
    treeHeight(tree) should equal(3)
  }

}
