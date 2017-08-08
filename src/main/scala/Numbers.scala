import scala.collection.mutable.ListBuffer

/**
  * Numbers problem
  * Write a program that outputs all possibilities to put + or - or nothing between
  * the numbers 1, 2, ..., 9 (in this order)
  * such that the result is always 100.
  * For example: 1 + 2 + 34 – 5 + 67 – 8 + 9 = 100.
  */
object Numbers {
  var digits = List(1,2,3,4,5,6,7,8,9)
  val searchSum = 100

  def concatPrefix(prefix:String, paths:List[String]): List[String] = {
    paths.filter(r =>r.length>0).map(r => prefix ++ r)
  }


  def findPaths(sum:Int, previousNumber:Int, index:Int):List[String] = {
    var previousDigit = Math.abs(previousNumber % 10)
    if (index >= digits.length) {
      if (sum == previousNumber) {
        return List(previousDigit.toString)
      }
      return List("")
    }

    var currentDigit = digits(index)
    var concatenatedNumber = 0
    if (previousNumber >= 0)
      concatenatedNumber = 10 * previousNumber + currentDigit
    else
      concatenatedNumber = 10 * previousNumber - currentDigit

    var paths = new ListBuffer[String]
    val func1 = concatPrefix(previousDigit.toString + "+", findPaths(sum-previousNumber, currentDigit, index+1))
    paths ++=func1
    val func2 = concatPrefix(previousDigit.toString + "-", findPaths(sum-previousNumber, -currentDigit, index+1))
    paths ++=func2
    val func3 = concatPrefix(previousDigit.toString + "&", findPaths(sum, concatenatedNumber, index+1))
    paths ++= func3
    paths.toList
  }

  /**
    * Main method
    *
    * @param args
    */
  def main(args: Array[String]): Unit = {
    val res = findPaths(searchSum, digits(0), 1)
    res.foreach(r=>{
      println(r.replace("&",""))
    })
  }
}

