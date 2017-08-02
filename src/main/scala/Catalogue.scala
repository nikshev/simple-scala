import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer

/**
  * Document class
  * @param title
  * @param year
  */
case class Document(title:String, year:Int)

/**
  * Catalogue class
  */
class Catalogue {
  var storage = new ListBuffer[Document] //Starage variable

  /**
    * Add document to storage
    * @param document
    */
  def add(document: Document): Unit = {
    storage += document
  }

  /**
    * Get documents from storage
    * @return
    */
  def getDocuments: List[Document] = {
    storage.toList
  }

  /**
    * Get sorted documents from storage
    * @return
    */
  def getSortedDocuments: List[Document] = {
    storage.toList.sortBy(r => r.title)
  }

  /**
    * Get unique words in title (worse solution)
    * @return
    */
  def getUniqueWords: List[String] = {
    val str = storage.toList.map(r => r.title).mkString(" ")
    str.split(" ").toList.distinct
  }

  /**
    * Get unique words in title (good solution)
    * @return
    */
  def getUniqueWords2: List[String] = {
    val str = storage.toList.map(r => r.title.split(" ")).flatMap(r => r)
    str.distinct
  }

  /**
    * Get documents count by year
    * @return
    */
  def getDocumentsCountByYear: Map[Int, Int] = {
    val tmp = storage.toList.map(r => r.year)
    tmp.groupBy(identity).mapValues(_.size)
  }

}

object Catalogue {

  /**
    * Main method
    *
    * @param args
    */
  def main(args: Array[String]): Unit = {
    val catalogue = new Catalogue
    catalogue.add(Document("First document", 1999))
    catalogue.add(Document("Second document", 1999))
    catalogue.add(Document("Third document", 1999))
    catalogue.add(Document("AThird document", 1999))
    catalogue.add(Document("AThird document", 2000))

    println(catalogue.getDocuments)
    println(catalogue.getSortedDocuments)
    println(catalogue.getUniqueWords)
    println(catalogue.getUniqueWords2)
    println(catalogue.getDocumentsCountByYear)
  }
}
