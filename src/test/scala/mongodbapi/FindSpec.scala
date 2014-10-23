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

  trait PropertyDocumentMetadata extends DocumentTypeMetadata[Property] {
    val name = new Field[String, BSONString]("name")
    val value = new Field[String, BSONString]("value")
  }

  implicit object PropertyDocumentMetadata extends PropertyDocumentMetadata

  trait CompanyDocumentMetadata extends DocumentTypeMetadata[Company] {
    val name = new Field[String, BSONString]("name")
    val country = new Field[String, BSONString]("country")
  }

  implicit object CompanyDocumentMetadata extends CompanyDocumentMetadata

  trait ProductDocumentMetadata extends DocumentTypeMetadata[Product] {
    val _id = new Field[Int, BSONInteger]("_id")
    val name = new Field[String, BSONString]("name")
    val properties = new ArrayField[List[Property], Property, PropertyDocumentMetadata, BSONDocument]("properties")
    val madeBy = new Field[Company, BSONDocument]("madeBy") with CompanyDocumentMetadata
    val sizes = new ArrayField[List[Int], Int, IntArrayElementTypeMetadata, BSONInteger]("sizes")
  }

  // We need to extend the ArrayField with this implicit class because we can't mixin PropertyDocumentMetadata.
  // That's because ArrayField (TypeMetadata[List[Property], BSONArray]) and PropertyDocumentMetadata
  // (TypeMetadata[Property, BSONDocument]) have different TypeMetadata's. But we also want to be able to call
  // all properties of the embedded type directly on the array: (array.embeddedField $in (1,2,3)).
  implicit class PropertyArray(a: ArrayField[List[Property], Property, PropertyDocumentMetadata, BSONDocument]) extends PropertyDocumentMetadata

  class ProductDocument(implicit writer: BSONWriter[Product, BSONDocument],
                        propertyWriter: BSONWriter[Property, BSONDocument],
                        companyWriter: BSONWriter[Company, BSONDocument])
    extends ProductDocumentMetadata

}

class FindSpec extends Specification with BSONDocumentMatchers {

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

  val queryGenerator = new QueryGenerator[ProductDocument](new ProductDocument)

  "Collection must find documents" in {

    val query = { product: ProductDocument =>
      (product.name -> "TV") &&
      (product.madeBy.name $in ("Samsung", "Philips")) &&
      (product.properties $elemMatch { property =>
        (property.name $eq "diagonal") &&
        (property.value $eq "40")
      }) &&
      (product.properties.value $eq "red") &&
      (product.sizes $eq 10) &&
      (product.sizes $eq List(10, 20, 30)) &&
      (product.sizes $elemMatch { _ $eq 10 })
    }

    val FindQuery(criteria, projection) = queryGenerator.find(query)

    ok
//
//
//    println(BSONDocument.pretty(criteria))
//
//    println(BSONDocument.pretty(BSONDocument(
//      "$and" -> BSONArray(
//        BSONDocument("name" -> "TV"),
//        BSONDocument("madeBy.name" -> BSONDocument("$in" -> BSONArray("Samsung", "Philips"))),
//        BSONDocument("properties" -> BSONDocument(
//          "$elemMatch" -> BSONDocument(
//            "$and" -> BSONArray(
//              BSONDocument("name" -> "diagonal"),
//              BSONDocument("value" -> "40")
//            ))
//        )),
//        BSONDocument("properties.value" -> "red")
//      )
//    )))
//
//    criteria must bsonEqualTo(BSONDocument(
//      "$and" -> BSONArray(
//        BSONDocument("name" -> "TV"),
//        BSONDocument("madeBy.name" -> BSONDocument("$in" -> BSONArray("Samsung", "Philips"))),
//        BSONDocument("properties" -> BSONDocument(
//          "$elemMatch" -> BSONDocument(
//            "$and" -> BSONArray(
//              BSONDocument("name" -> "diagonal"),
//              BSONDocument("value" -> "40")
//          ))
//        )),
//        BSONDocument("properties.value" -> "red")
//      )
//    ))
//
  }



}
