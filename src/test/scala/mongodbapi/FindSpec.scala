package mongodbapi

import org.scalatest._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
 * Abstracts for Document
 */


trait Expression {
  // syntactic sugar
  def &&(exp2: Expression) = Expression.$and(this, exp2)
  def ||(exp2: Expression) = Expression.$or(this, exp2)
}
object Expression {
  case class $eq[T](field: Field[T], value: T) extends Expression
  case class $in[T](field: Field[T], values: Iterable[T]) extends Expression
  case class $elemMatch[T](field: Field[T], exp: Expression) extends Expression

  case class $and[T](exp1: Expression, exp2: Expression) extends Expression
  case class $or[T](exp1: Expression, exp2: Expression) extends Expression
}

trait Document[T]

class Field[T](name: String) {
  def $eq(value: T) = Expression.$eq(this, value)
  def $in(values: Iterable[T]) = Expression.$in(this, values)
}

abstract class ArrayField[T, Self](name: String) extends Field[T](name) {
  val self: Self
  def $elemMatch(query: Self => Expression) = Expression.$elemMatch(this, query(self))
}

abstract class DocumentField[T, Self](name: String) extends Field[T](name) with Document[T] {
  val self: Self
}


/**
 * API
 */

class Collection[T, D <: Document[T]](collectionName: String)
                                     (implicit documentBuilder: DocumentBuilder[T, D]) {
  def find(query: D => Expression): Future[List[T]] = Future.successful(Nil)
}

trait DocumentBuilder[T, D <: Document[T]] {
  def build(obj: T): D
}

/**
 * Concrete domain
 */

case class Product(_id: Int, name: String, properties: List[Property], madeBy: Company)
case class Property(name: String, value: String)
case class Company(name: String, country: String)

/**
 * Macro-Generated Documents
 */

class ProductDocument extends Document[Product] {
  val _id = new Field[Int]("_id")
  val name = new Field[String]("name")
  val properties = new PropertyArray
  val madeBy = new CompanyDocument
}

class PropertyArray extends ArrayField[Property, PropertyArray]("properties") {
  val self: PropertyArray = this
  val name = new Field[String]("name")
  val value = new Field[String]("value")
}

class CompanyDocument extends DocumentField[Company, CompanyDocument]("madeBy") {
  val self: CompanyDocument = this
  val name = new Field[String]("name")
  val country = new Field[String]("country")
}

class FindSpec extends FlatSpec with Matchers {

  def resultOf[T](fut: Future[T]): T = Await.result(fut, Duration(60, SECONDS))

  "Collection" must "find documents" in {

    val products = {
      implicit val productDocBuilder = new DocumentBuilder[Product, ProductDocument] {
        def build(obj: Product) = new ProductDocument
      }
      new Collection[Product, ProductDocument]("products")
    }

    val result = products.find({ product =>
      (product.name $eq "TV") &&
      (product.madeBy.name $in (List("Samsung", "Philips"))) &&
      (product.properties $elemMatch { property =>
        (property.name $eq "diagonal") &&
        (property.value $eq "40")
      })
    })

    resultOf(result) shouldEqual Nil

  }



}
