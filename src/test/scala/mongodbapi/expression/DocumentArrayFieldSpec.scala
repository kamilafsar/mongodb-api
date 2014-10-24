package mongodbapi.expression

import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Properties}
import reactivemongo.bson._

object DocumentArrayFieldSpec extends Properties("ArrayField[List[MyClass], MyClass, DocumentTypeMetadata[MyClass], BSONDocument]") {

  import mongodbapi._

  case class MyClass(myProperty: String)
  
  implicit val myClassHandler = Macros.handler[MyClass]

  class MyClassMetadata(parent: Option[BaseField]) extends DocumentTypeMetadata[MyClass] {
    val myProperty = new Field[String, BSONString]("myProperty", parent)
  }

  implicit object MyClassMetadata extends MyClassMetadata(None)

  implicit val genMyClass = Arbitrary[MyClass] {
    for (myProperty <- Arbitrary.arbitrary[String])
      yield MyClass(myProperty)
  }

  protected def fieldWithName(fieldName: String): ArrayField[List[MyClass], MyClass, MyClassMetadata, BSONDocument] = {
    new ArrayField[List[MyClass], MyClass, MyClassMetadata, BSONDocument](fieldName, None)
  }

  /**
   * Comparison expressions
   */

  property("$eq") = forAll { (fieldName: String, fieldValue: MyClass) =>
    val field = fieldWithName(fieldName)
    (field $eq fieldValue).toBSON == BSONDocument(fieldName -> fieldValue)
  }

  property("$eq") = forAll { (fieldName: String, fieldValue: List[MyClass]) =>
    val field = fieldWithName(fieldName)
    (field $eq fieldValue).toBSON == BSONDocument(fieldName -> fieldValue)
  }

  property("->") = forAll { (fieldName: String, fieldValue: MyClass) =>
    val field = fieldWithName(fieldName)
    (field -> fieldValue).toBSON == BSONDocument(fieldName -> fieldValue)
  }

  property("->") = forAll { (fieldName: String, fieldValue: List[MyClass]) =>
    val field = fieldWithName(fieldName)
    (field -> fieldValue).toBSON == BSONDocument(fieldName -> fieldValue)
  }

  property("$ne") = forAll { (fieldName: String, fieldValue: MyClass) =>
    val field = fieldWithName(fieldName)
    (field $ne fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$ne" -> fieldValue))
  }

  property("$ne") = forAll { (fieldName: String, fieldValue: List[MyClass]) =>
    val field = fieldWithName(fieldName)
    (field $ne fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$ne" -> fieldValue))
  }

  property("$in") = forAll { (fieldName: String, fieldValue: List[MyClass]) =>
    val field = fieldWithName(fieldName)
    (field $in fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$in" -> fieldValue))
  }

  property("$in") = forAll { (fieldName: String, fieldValue: List[List[MyClass]]) =>
    val field = fieldWithName(fieldName)
    (field $in fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$in" -> fieldValue))
  }

  property("$nin") = forAll { (fieldName: String, fieldValue: List[MyClass]) =>
    val field = fieldWithName(fieldName)
    (field $nin fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$nin" -> fieldValue))
  }

  property("$nin") = forAll { (fieldName: String, fieldValue: List[List[MyClass]]) =>
    val field = fieldWithName(fieldName)
    (field $nin fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$nin" -> fieldValue))
  }

  /**
   * Element expressions
   */

  property("$exists") = forAll { (fieldName: String, exists: Boolean) =>
    val field = fieldWithName(fieldName)
    (field $exists exists).toBSON == BSONDocument(fieldName -> BSONDocument("$exists" -> BSONBoolean(exists)))
  }

  /**
   * Array expressions
   */

  property("$all") = forAll { (fieldName: String, fieldValue: List[MyClass]) =>
    val field = fieldWithName(fieldName)
    (field $all fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$all" -> fieldValue))
  }

  property("$elemMatch") = forAll { (fieldName: String, fieldValue: MyClass) =>
    val field = fieldWithName(fieldName)
    (field $elemMatch { x => x -> fieldValue }).toBSON == BSONDocument(fieldName -> BSONDocument("$elemMatch" -> BSONDocument("$eq" -> fieldValue)))
  }

  property("$size") = forAll { (fieldName: String, size: Int) =>
    val field = fieldWithName(fieldName)
    (field $size size).toBSON == BSONDocument(fieldName -> BSONDocument("$size" -> BSONInteger(size)))
  }

  property("$size") = forAll { (fieldName: String, size: Long) =>
    val field = fieldWithName(fieldName)
    (field $size size).toBSON == BSONDocument(fieldName -> BSONDocument("$size" -> BSONLong(size)))
  }

}

