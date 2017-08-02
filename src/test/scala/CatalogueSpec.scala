import Domino.solution
import org.scalatest._

import scala.collection.immutable.List

class CatalogueSpec extends FlatSpec with Matchers {

  val catalogue = new Catalogue
  catalogue.add(Document("First document", 1999))
  catalogue.add(Document("Second document", 1999))
  catalogue.add(Document("Third document", 1999))
  catalogue.add(Document("AThird document", 1999))
  catalogue.add(Document("AThird document", 2000))

  "Catalogue getDocuments" should " return correct list" in {
    catalogue.getDocuments should equal(List(Document("First document", 1999),
      Document("Second document", 1999),
      Document("Third document", 1999),
      Document("AThird document", 1999),
      Document("AThird document", 2000))
    )
  }

  "Catalogue getSortedDocuments" should " return correct list" in {
    catalogue.getSortedDocuments should equal(List(Document("AThird document", 1999),
      Document("AThird document", 2000),
      Document("First document", 1999),
      Document("Second document", 1999),
      Document("Third document", 1999))
    )
  }

  "Catalogue getUniqueWords" should " return correct list" in {
    catalogue.getUniqueWords should equal(List("First", "document", "Second", "Third", "AThird"))
  }

  "Catalogue getUniqueWords2" should " return correct list" in {
    catalogue.getUniqueWords2 should equal(List("First", "document", "Second", "Third", "AThird"))
  }

  "Catalogue.getDocumentsCountByYear" should " return correct list" in {
    catalogue.getDocumentsCountByYear should equal(Map(2000 -> 1, 1999 -> 4))
  }

}
