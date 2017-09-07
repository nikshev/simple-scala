import scala.collection.mutable

/**
  * Braces task
  * Check string which contain braces
  */
object Braces {
  private val stack = new mutable.Stack[Char]

  def checkString(str: String, rules: Map[Char, Char]): Boolean = {
    stack.clear()
    str.toList.foreach(chr => {
      if (!stack.isEmpty) {
        val before = stack.pop()
        if (chr!=rules(before)) {
          stack.push(before)
          stack.push(chr)
        }
      } else
        stack.push(chr)
     }
    )
    stack.isEmpty
  }

  def main(args: Array[String]) = {
    println("Hello Scala!")
    println(checkString("{}{{}}{",Map('{'->'}')))
  }
}
