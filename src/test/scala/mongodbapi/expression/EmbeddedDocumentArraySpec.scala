package mongodbapi.expression

import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Properties}
import reactivemongo.bson._

object EmbeddedDocumentArraySpec extends Properties("ArrayField[List[MyClass2], MyClass2, DocumentTypeMetadata[MyClass2], BSONDocument]") {

  import mongodbapi._

  case class Product(_id: Int, properties: List[Property])
  case class Property(name: String, value: String, propertyType: PropertyType)
  case class PropertyType(name: String)

  implicit val propertyTypeHandler = Macros.handler[PropertyType]
  implicit val propertyHandler = Macros.handler[Property]
  implicit val productHandler = Macros.handler[Product]

  trait PropertyTypeDocumentMetadata extends DocumentTypeMetadata[PropertyType] {
    val name = new Field[String, BSONString]("name")
  }

  implicit object PropertyTypeDocumentMetadata extends PropertyTypeDocumentMetadata

  trait PropertyDocumentMetadata extends DocumentTypeMetadata[Property] {
    val name = new Field[String, BSONString]("name")
    val value = new Field[String, BSONString]("value")
    val propertyType = new Field[PropertyType, BSONDocument]("propertyType") with PropertyTypeDocumentMetadata
  }

  implicit object PropertyDocumentMetadata extends PropertyDocumentMetadata

  trait ProductDocumentMetadata extends DocumentTypeMetadata[Product] {
    val _id = new Field[Int, BSONInteger]("_id")
    val properties = new ArrayField[List[Property], Property, PropertyDocumentMetadata, BSONDocument]("properties")
  }

  implicit class PropertyArray(a: ArrayField[List[Property], Property, PropertyDocumentMetadata, BSONDocument]) extends PropertyDocumentMetadata

  class ProductDocument(implicit writer: BSONWriter[Product, BSONDocument],
                        propertyWriter: BSONWriter[Property, BSONDocument],
                        propertyTypeWriter: BSONWriter[PropertyType, BSONDocument])
    extends ProductDocumentMetadata


  implicit val genProperty = Arbitrary[Property] {
    for {
      name <- Arbitrary.arbitrary[String]
      value <- Arbitrary.arbitrary[String]
      propertyType <- Arbitrary.arbitrary[String]
    } yield Property(name, value, PropertyType(propertyType))
  }

  implicit val genProduct = Arbitrary[Product] {
    for {
      id <- Arbitrary.arbitrary[Int]
      propArr <- Arbitrary.arbitrary[List[Property]]
    } yield Product(id, propArr)
  }


  property("->") = forAll { (fieldValue: String) =>

    val expr = ({ product: ProductDocument =>
      product.properties.propertyType.name $eq fieldValue
    })(new ProductDocument)

    val expected = BSONDocument(s"properties.propertyType.name" -> fieldValue)

    println(BSONDocument.pretty(expr.toBSON))
    println(BSONDocument.pretty(expected))

    expr.toBSON == expected
  }

  property("$elemMatch") = forAll { (propertyNameValue: String) =>

    val expr = ({ product: ProductDocument =>
      product.properties $elemMatch { property =>
        property.name $eq propertyNameValue
      }
    })(new ProductDocument)


    val expected = BSONDocument("properties" -> BSONDocument("$elemMatch" -> BSONDocument("name" -> propertyNameValue)))

    expr.toBSON == expected
  }

  property("$elemMatch") = forAll { (propertyTypeValue: String) =>

    val expr = ({ product: ProductDocument =>
      product.properties $elemMatch { property =>
        property.propertyType.name $eq propertyTypeValue
      }
    })(new ProductDocument)


    val expected = BSONDocument("properties" -> BSONDocument("$elemMatch" -> BSONDocument("propertyType.name" -> propertyTypeValue)))

//    println(BSONDocument.pretty(expr.toBSON))
//    println(BSONDocument.pretty(expected))
    expr.toBSON == expected
  }

}

