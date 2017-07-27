import scala.collection.mutable.ListBuffer

/**
  * Fibonacci object (recursive variant)
  */
object Fibonacci {

  /**
    * Get number from Fibanacci series
    *
    * @param term     :Int
    * @param value    :Int
    * @param previous :Int
    * @return Int
    */
  def fibonacci(term: Int, value: Int, previous: Int = 0): Int = {
    val result = term match {
      case 0 | 1 => value
      case _ => fibonacci(term - 1, value + previous, value)
    }
    result
  }

  /**
    * Generate Fibonacci sereis
    *
    * @param start - start for count
    * @param end   - end for count
    * @return List
    */
  def generate(start: Int, end: Int): List[Int] = {
    var results = new ListBuffer[Int]
    for (i <- start to end) {
      results += fibonacci(i, 0, 1)
    }
    results.toList
  }

  /**
    * Main method
    *
    * @param args
    */
  def main(args: Array[String]): Unit = {
    println(generate(5, 15))
  }
}
