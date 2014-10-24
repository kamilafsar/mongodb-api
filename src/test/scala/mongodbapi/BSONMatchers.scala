package mongodbapi

import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification
import reactivemongo.bson.{BSONArray, BSONValue, BSONDocument}

trait BSONMatchers {
  self: Specification =>

  def toList(doc: BSONDocument): List[(String, BSONValue)] = {
    def toList(bson: BSONValue, prefix: String): List[(String, BSONValue)] = bson match {
      case doc: BSONDocument =>
        doc.elements.flatMap {
          case (key, value) => toList(value, prefix + "." + key)
        }.toList
      case arr: BSONArray =>
        arr.values.zipWithIndex.flatMap {
          case (value, idx) => toList(value, prefix + "." + idx)
        }.toList
      case other =>
        List(prefix -> other)
    }
    toList(doc, "")
  }

  /**
   * BSONDocumenten zijn 'ordered'. In mongo is { aap: 1, noot: 1 } niet gelijk aan { noot: 1, aap: 1}.
   * Met reactivemongo heb je daar heel weinig last van door het (de)serializen van de BSONHandlers.
   *
   * Maar bij unittests willen we dit verschil soms niet zien. Een Map[String, ...] is niet ordered waardoor
   * de equals method conceptueel logischer werkt.
   *
   * TODO: this will only work for BSONDocuments, not BSONArray with objects within it
   */
  def bsonEqualTo(doc2: BSONDocument): Matcher[BSONDocument] = {
    ((doc1: BSONDocument) => toList(doc1)) ^^ beEqualTo(toList(doc2))
  }


}
