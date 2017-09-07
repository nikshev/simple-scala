import scala.annotation.tailrec

/**
  * Created by yshkurnykov on 31.07.2017.
  */
/**
  * Extended Integer Array class
  * @param a:Array[Int]
  */
class ExtendedIntArray(a:Array[Int]) {

  /**
    * Maximum element in array
    * @return
    */
  def maxEl:Int ={
    var max = 0
    a.foreach(el=>{
      if (el>max)
        max=el
    })
    max
  }

  /**
    * Minimu element in array
    * @return
    */
  def minEl:Int ={
    var min =a(0)
    a.foreach(el=>{
      if (min>el)
        min = el
    })
    min
  }

  /**
    * Array elements sum
    * @return
    */
  def sumEl:Int ={
    def count(summ:Int,ar:List[Int]):Int = {
      ar match {
        case xs::tail => count(summ+xs,tail)
        case Nil => summ
      }
    }
    count(0,a.toList)
  }

  /**
    * Size of array
    * @return
    */
  def sizeAr:Int = {
    def count(summ:Int,ar:List[Int]):Int = {
      ar match {
        case xs::tail => count(summ+1,tail)
        case Nil => summ
      }
    }
    count(0,a.toList)
  }

  /**
    * Mean of array elements
    * @return
    */
  def mean:Double ={
    sumEl.toDouble/sizeAr.toDouble
  }

  /**
    * Median of array elements
    * @return
    */
  def median:Double = {
    a.sortWith(_ < _).drop(a.length/2).head
  }

  /**
    * More frequent element
    * @return
    */
  def moreFreq={
    a.groupBy(identity).mapValues(_.size).maxBy(_._2)
  }

  /**
    * Less frequent element
    * @return
    */
  def lessFreq={
    a.groupBy(identity).mapValues(_.size).minBy(_._2)
  }

  /**
    * Standars deviation
    * @return
    */
  def stdDev = {
    Math.sqrt((a.map( _ - mean).map(t => t*t).sum)/a.length)
  }

  /**
    * List reverse
    * @param l
    * @return
    */
  def reverseList(l: List[Int]): List[Int] = l match {
    case h :: tail => reverseList(tail) ::: List(h)
    case Nil => Nil
  }

  /**
    * List reverse with tail rec
    * @param l
    * @return
    */
  def reverseListTailRec(l: List[Int]): List[Int] = {
    @tailrec
    def _reverse(res: List[Int], rem: List[Int]): List[Int] = rem match {
      case Nil => res
      case h :: tail => _reverse(h :: res, tail)
    }
    _reverse(Nil, l)
  }

  /**
    * Array reverse (technical method)
    * @return
    */
  def reverse = {
    reverseList(a.toList).toArray
  }

  /**
    * Array reverse (technical method) with tailrec
    * @return
    */
  def reverseWithTail = {
    reverseListTailRec(a.toList).toArray
  }
  /**
    * Array describe method
    */
  def describe ={
    print("Array:")
    println(a.mkString(","))
    print("Array reversed:")
    println(reverse.mkString(","))
    print("Array reversed 2:")
    println(reverseWithTail.mkString(","))
    print("Max element:")
    println(maxEl)
    print("Min element:")
    println(minEl)
    print("Sum of elements:")
    println(sumEl)
    print("Count of elements:")
    println(sizeAr)
    print("Mean:")
    println(mean)
    print("Median:")
    println(median)
    print("Standard deviation:")
    println(stdDev)
    print("More frequent element is:")
    println(moreFreq._1)
    print("More frequent element appears:")
    println(moreFreq._2)
    print("Less frequent element is:")
    println(lessFreq._1)
    print("Less frequent element appears:")
    println(lessFreq._2)
  }

}

/**
  * Obkject for working with array
  */
object ExtendedIntArray {
  implicit def extendedIntArray(a:Array[Int]) = new ExtendedIntArray(a)

  /**
    * Main method with some test cases
    * @param args
    */
  def main(args: Array[String]) = {
    println("Array task!")
    val a = Array(2,1,2,4,2,6,7,8,9,10,0)
    a.describe
  }
}
