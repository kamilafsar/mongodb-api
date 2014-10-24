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

  class PropertyTypeDocumentMetadata(parent: Option[BaseField]) extends DocumentTypeMetadata[PropertyType] {
    val name = new Field[String, BSONString]("name", parent)
  }

  class PropertyDocumentMetadata(parent: Option[BaseField]) extends DocumentTypeMetadata[Property] {
    val name = new Field[String, BSONString]("name", parent)
    val value = new Field[String, BSONString]("value", parent)
    val propertyType = new Field[PropertyType, BSONDocument]("propertyType", parent)
  }

  class ProductDocumentMetadata(parent: Option[BaseField]) extends DocumentTypeMetadata[Product] {
    val _id = new Field[Int, BSONInteger]("_id", parent)
    val properties = new ArrayField[List[Property], Property, PropertyDocumentMetadata, BSONDocument]("properties", parent)
    val upPropertyType = new Field[PropertyType, BSONDocument]("propertyType", parent)
  }


  implicit object PropertyTypeDocumentMetadata extends PropertyTypeDocumentMetadata(None)
  implicit object PropertyDocumentMetadata extends PropertyDocumentMetadata(None)


  implicit class PropertyArray(a: ArrayField[List[Property], Property, PropertyDocumentMetadata, BSONDocument]) extends PropertyDocumentMetadata(Some(a))

  implicit class PropertyTypeDocument(a: Field[PropertyType, BSONDocument]) extends PropertyTypeDocumentMetadata(Some(a))


  class ProductDocument(implicit writer: BSONWriter[Product, BSONDocument],
                        propertyWriter: BSONWriter[Property, BSONDocument],
                        propertyTypeWriter: BSONWriter[PropertyType, BSONDocument])
    extends ProductDocumentMetadata(None)


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

    expr.toBSON == expected
  }

}

