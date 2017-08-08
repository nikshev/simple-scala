import scala.collection.mutable.ListBuffer

/**
  * Find prime numbers
  * Please write a function that takes one parameter (limit) and returns all prime numbers up to that limit
  * E.g. limit = 100, result = [2, 3, 5, 7, 11, ..., 97]
  */
object Prime {
  def isPrime(i:Int):Boolean = {
    if (i<=1)
      false
    else if (i==2)
      true
    else
      !(2 to (i-1)).exists(x=>i % x==0)
  }

  def primeNumbers(limit:Int): List[Int] = {
    var results = new ListBuffer[Int]
    for (i <- 0 to limit) {
      if (isPrime(i))
        results += i
    }
    results.toList
  }

  def primeNumbers2(limit: Int): List[Int] = (2 to limit).filter(i=>isPrime(i)).toList

  def main(args: Array[String]) = {
    println("Hello Scala!")
    println(primeNumbers(100))
    println(primeNumbers2(100))
  }
}
