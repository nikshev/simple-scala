object Tree {
  sealed trait btree[+A]
  case object Empty extends btree[Nothing]
  case class Node[+A](elem: A, left : btree[A], right : btree[A]) extends btree[A]

  def list2btree(list : List[Int]) : btree[Int] = {
    def insert2tree(x : Int, t : btree[Int]) : btree[Int] = {
      (x, t) match {
        case (x, Empty) => Node(x, Empty, Empty)
        case (x, Node(v, left, right)) => {
          if(v > x) Node(v, insert2tree(x, left), right)
          else if(v < x) Node(v, left, insert2tree(x,right))
          else throw new Exception("Element exsist!")
        }
      }
    }
    list match {
      case Nil => Empty
      case (h::t) => insert2tree(h, list2btree(t))
    }
  }

  def sizeTree(tree:btree[Int]):Int = {
    def size (sum: Int, tree:btree[Int]):Int = {
      tree match {
        case Node(e,l,r) => size(sum,l)+size(sum,r)+1
        case Empty => sum
      }
    }
    size(0,tree)
  }

  def nodeElementsSum(tree:btree[Int]):Int = {
    def total (sum: Int, tree:btree[Int]):Int = {
      tree match {
        case Node(e,l,r) => e+total(e,l)+total(e,r)
        case Empty => 0
      }
    }
    total(0,tree)
  }

  def treeHeight(tree:btree[Int]):Int = {
    def size (sum: Int, tree:btree[Int]):Int = {
      tree match {
        case Node(e,l,r) => size(sum,l)+size(sum,r)+1
        case Empty => sum
      }
    }

    tree match {
      case Node(e,l,r) => size(0,l) max size(0,r)
      case Empty => 0
    }
  }

  def main(args: Array[String]) = {
    println("Hello Tree!")
    val tree=list2btree(List(6,2,3,1,4))
    print("Tree:")
    println(tree)
    print("Tree size:")
    println(sizeTree(tree))
    print("Tree elements sum")
    println(nodeElementsSum(tree))
    print("Tree height:")
    println(treeHeight(tree))
  }
}