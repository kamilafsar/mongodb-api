package tests

import mongodbapi._
import reactivemongo.bson._
import org.specs2.mutable.Specification
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
 * Concrete domain
 */

case class Product(_id: Int, name: String, properties: List[Property], madeBy: Company, sizes: List[Int])
case class Property(name: String, value: String)
case class Company(name: String, country: String)

/**
 * Macro-Generated Documents
 */
object generated {

  class PropertyDocumentMetadata(parent: Option[BaseField]) extends DocumentTypeMetadata[Property] {
    val name = new Field[String, BSONString]("name", parent)
    val value = new Field[String, BSONString]("value", parent)
  }

  implicit object PropertyDocumentMetadata extends PropertyDocumentMetadata(None)

  class CompanyDocumentMetadata(parent: Option[BaseField]) extends DocumentTypeMetadata[Company] {
    val name = new Field[String, BSONString]("name", parent)
    val country = new Field[String, BSONString]("country", parent)
  }

  implicit object CompanyDocumentMetadata extends CompanyDocumentMetadata(None)

  class ProductDocumentMetadata(parent: Option[BaseField]) extends DocumentTypeMetadata[Product] {
    val _id = new Field[Int, BSONInteger]("_id", parent)
    val name = new Field[String, BSONString]("name", parent)
    val properties = new ArrayField[List[Property], Property, PropertyDocumentMetadata, BSONDocument]("properties", parent)
    val madeBy = new Field[Company, BSONDocument]("madeBy", parent)
    val sizes = new ArrayField[List[Int], Int, IntArrayElementTypeMetadata, BSONInteger]("sizes", parent)
  }

  implicit object ProductDocument extends ProductDocumentMetadata(None)

  // We need to extend the ArrayField with this implicit class because we can't mixin PropertyDocumentMetadata.
  // That's because ArrayField (TypeMetadata[List[Property], BSONArray]) and PropertyDocumentMetadata
  // (TypeMetadata[Property, BSONDocument]) have different TypeMetadata's. But we also want to be able to call
  // all properties of the embedded type directly on the array: (array.embeddedField $in (1,2,3)).
  implicit class PropertyArray(a: ArrayField[List[Property], Property, PropertyDocumentMetadata, BSONDocument]) extends PropertyDocumentMetadata(Some(a))
  implicit class MadeByDocument(a: Field[Company, BSONDocument]) extends CompanyDocumentMetadata(Some(a))

}

class FindSpec extends Specification with BSONMatchers {

  /**
   * I want this syntax:
   *
   * val result = products.find({ product =>
   *   (product.name $eq "TV") &&
   *   (product.madeBy.name $in (List("Samsung", "Philips"))) &&
   *   (product.properties $elemMatch { property =>
   *     (property.name $eq "diagonal") &&
   *     (property.value $eq "40")
   *   })
   * })
   *
   */

  import generated._

  def resultOf[T](fut: Future[T]): T = Await.result(fut, Duration(60, SECONDS))

  implicit val propertyWriter = Macros.handler[Property]
  implicit val companyWriter = Macros.handler[Company]
  implicit val productWriter = Macros.handler[Product]

  val queryGenerator = new QueryGenerator[Product]

  "Collection must find documents" in {

    val query = { product: ProductDocumentMetadata =>
      (product.name -> "TV") &&
      (product.madeBy.name $in ("Samsung", "Philips")) &&
      (product.properties $elemMatch { property =>
        (property.name -> "diagonal") &&
        (property.value -> "40")
      }) &&
      (product.properties.value -> "red") &&
      (product.sizes -> 10) &&
      (product.sizes $in List(10, 20, 30)) &&
      (product.sizes -> List(10, 20, 30)) &&
      (product.sizes $elemMatch { size => (size $gt 10) && (size $lt 20) })
    }

    val FindQuery(criteria, projection) = queryGenerator.find(query)

    val expected = BSONDocument(
      "$and" -> BSONArray(
        BSONDocument("name" -> "TV"),
        BSONDocument("madeBy.name" -> BSONDocument("$in" -> BSONArray("Samsung", "Philips"))),
        BSONDocument("properties" -> BSONDocument(
          "$elemMatch" -> BSONDocument(
            "$and" -> BSONArray(
              BSONDocument("name" -> "diagonal"),
              BSONDocument("value" -> "40")
          ))
        )),
        BSONDocument("properties.value" -> "red"),
        BSONDocument("sizes" -> BSONInteger(10)),
        BSONDocument("sizes" -> BSONDocument("$in" -> BSONArray(BSONInteger(10), BSONInteger(20), BSONInteger(30)))),
        BSONDocument("sizes" -> BSONArray(BSONInteger(10), BSONInteger(20), BSONInteger(30))),
        BSONDocument("sizes" -> BSONDocument("$elemMatch" -> BSONDocument(
        "$and" -> BSONArray(
          BSONDocument("$gt" -> BSONInteger(10)),
          BSONDocument("$lt" -> BSONInteger(20))
        ))))
      )
    )

    criteria must bsonEqualTo(expected)
  }



}
