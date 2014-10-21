package mongodbapi

import reactivemongo.bson._
import org.specs2.mutable.Specification
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
 * Concrete domain
 */

case class Product(_id: Int, name: String, properties: List[Property], madeBy: Company)
case class Property(name: String, value: String)
case class Company(name: String, country: String)

/**
 * Macro-Generated Documents
 */

object ImplicitWriters {
  implicit val propertyWriter = Macros.handler[Property]
  implicit val companyWriter = Macros.handler[Company]
  implicit val productWriter = Macros.handler[Product]
}

import ImplicitWriters._

class ProductDocument extends Document[Product] {
  val _id = new Field[Int, BSONInteger]("_id", None)
  val name = new Field[String, BSONString]("name", None)
  val properties = new ArrayField[List[Property], PropertyDocument.type, Property, BSONDocument]("properties", None, PropertyDocument) {}
  val madeBy = CompanyDocument
}

object PropertyDocument extends DocumentField[Property]("properties", None) {
  val name = new Field[String, BSONString]("name", subfieldAncestors)
  val value = new Field[String, BSONString]("value", subfieldAncestors)
}

object CompanyDocument extends DocumentField[Company]("madeBy", None) {
  val name = new Field[String, BSONString]("name", subfieldAncestors)
  val country = new Field[String, BSONString]("country", subfieldAncestors)
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


  def resultOf[T](fut: Future[T]): T = Await.result(fut, Duration(60, SECONDS))

  val queryGenerator = {
    implicit val productDocBuilder = new DocumentBuilder[Product, ProductDocument] {
      def build[Product] = new ProductDocument
    }
    new QueryGenerator[Product, ProductDocument]
  }

  "Collection must find documents" in {

    val query = { product: ProductDocument =>
      (product.name $eq "TV") &&
      (product.madeBy.name $in (List("Samsung", "Philips"))) &&
      (product.properties $elemMatch { property =>
        (property.name $eq "diagonal") &&
        (property.value $eq "40")
      })
    }

    val FindQuery(criteria, projection) = queryGenerator.find(query)


    println(BSONDocument.pretty(criteria))



    criteria must bsonEqualTo(BSONDocument(
      "$and" -> BSONArray(
        BSONDocument("name" -> "TV"),
        BSONDocument("madeBy.name" -> BSONDocument("$in" -> BSONArray("Samsung", "Philips"))),
        BSONDocument("properties" -> BSONDocument(
          "$elemMatch" -> BSONDocument(
            "name" -> "diagonal",
            "value" -> "40"
          )
        ))
      )
    ))

  }



}
