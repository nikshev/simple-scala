import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer

/**
  * Domino task
  * We need to create chain from domino pieces
  * For example pieces:
  * (1,2)
  * (1,3)
  * (2,6)
  * (3,6)
  * (5,6)
  * To chain:
  * 56:63:31:12:26
  */

object Domino {

   //Chain variable
   var chain = new ListBuffer[List[Int]]

  /**
    * Get pair for piece
    * @param piece
    * @param pieces
    * @return List
    */
   def getPair(piece:List[Int], pieces:List[List[Int]]):List[List[Int]] = {
     var results=new ListBuffer[List[Int]]
     pieces.foreach(row =>
        if (row(1) == piece(0))
          results += row.reverse
     )
     return results.toList
   }

  /**
    * Get pair for reverse piece
    * @param piece
    * @param pieces
    * @return
    */
  def getReversePair(piece:List[Int], pieces:List[List[Int]]):List[List[Int]] = {
    var results=new ListBuffer[List[Int]]
    pieces.foreach(row =>
      if (row(0) == piece(1))
        results += row
    )
    return results.toList
  }

  /**
    * Add to chain tail
    * @param piece
    * @param pieces
    * @return
    */
   def addToEndChain(piece:List[Int], pieces:List[List[Int]]):Boolean ={
     var pairs = getPair(piece,pieces.filterNot(p => p == piece || p == piece.reverse))
     if (pairs.size==0) {
       pairs = getReversePair(piece, pieces.filterNot(p => p == piece || p == piece.reverse))
     }
     pairs.foreach(pair => {
       if (chain.filterNot(p => p == pair || p == pair.reverse).length == chain.length) {
         chain += pair
         return true
       }
     })

     return false
   }

  /**
    * Add to chain head
    * @param piece
    * @param pieces
    * @return
    */
  def addToBeginChain(piece:List[Int], pieces:List[List[Int]]):Boolean ={
    var pairs = getPair(piece,pieces.filterNot(p => p == piece || p == piece.reverse))
    if (pairs.size==0) {
      pairs = getReversePair(piece, pieces.filterNot(p => p == piece || p == piece.reverse))
    }
    pairs.foreach(pair => {
      if (chain.filterNot(p => p == pair || p == pair.reverse).length == chain.length) {
        chain = pair.reverse +: chain
        return true
      }
    })

    return false
  }

  /**
    * Solution method
    * @param pieces
    * @return
    */
   def solution(pieces:List[List[Int]]):String ={
     chain.clear()
     val clean_pieces=pieces.distinct
     var str=""
     chain+=clean_pieces(0)
      for(i<-0 to clean_pieces.length-1){
         var piece=chain.toList.last
         if (!addToEndChain(piece,clean_pieces)) {
           var piece=chain.toList(0)
            if (!addToBeginChain(piece.reverse,clean_pieces))
              addToBeginChain(piece,clean_pieces)
         }
       }
     chain.toList.foreach(l =>
       str+=l.mkString("")+":"
     )
     str.substring(0,str.length-1)
   }

  /**
    * Main method
    *
    * @param args
    */
  def main(args: Array[String]): Unit = {
     //Some test cases
     println(solution(List(List(1,2),List(1,3),List(2,6),List(3,6),List(5,6),List(5,6))))
     println(solution(List(List(1,2),List(1,3),List(2,6),List(3,6),List(4,7))))
  }
}
