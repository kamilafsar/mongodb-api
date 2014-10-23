package mongodbapi.expression

import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Properties}
import reactivemongo.bson._

object DocumentArrayFieldSpec extends Properties("ArrayField[List[MyClass], MyClass, DocumentTypeMetadata[MyClass], BSONDocument]") {

  import mongodbapi._

  case class MyClass(myProperty: String)
  
  implicit val myClassHandler = Macros.handler[MyClass]

  trait MyClassMetadata extends DocumentTypeMetadata[MyClass] {
    val myProperty = new Field[String, BSONString]("myProperty")
  }

  implicit object MyClassMetadata extends MyClassMetadata

  implicit val genMyClass = Arbitrary[MyClass] {
    for (myProperty <- Arbitrary.arbitrary[String])
      yield MyClass(myProperty)
  }


  /**
   * Comparison expressions
   */

  property("$eq") = forAll { (fieldName: String, fieldValue: MyClass) =>
    val field = new ArrayField[List[MyClass], MyClass, MyClassMetadata, BSONDocument](fieldName)
    (field $eq fieldValue).toBSON == BSONDocument(fieldName -> fieldValue)
  }

  property("$eq") = forAll { (fieldName: String, fieldValue: List[MyClass]) =>
    val field = new ArrayField[List[MyClass], MyClass, MyClassMetadata, BSONDocument](fieldName)
    (field $eq fieldValue).toBSON == BSONDocument(fieldName -> fieldValue)
  }

  property("->") = forAll { (fieldName: String, fieldValue: MyClass) =>
    val field = new ArrayField[List[MyClass], MyClass, MyClassMetadata, BSONDocument](fieldName)
    (field -> fieldValue).toBSON == BSONDocument(fieldName -> fieldValue)
  }

  property("->") = forAll { (fieldName: String, fieldValue: List[MyClass]) =>
    val field = new ArrayField[List[MyClass], MyClass, MyClassMetadata, BSONDocument](fieldName)
    (field -> fieldValue).toBSON == BSONDocument(fieldName -> fieldValue)
  }

  property("$ne") = forAll { (fieldName: String, fieldValue: MyClass) =>
    val field = new ArrayField[List[MyClass], MyClass, MyClassMetadata, BSONDocument](fieldName)
    (field $ne fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$ne" -> fieldValue))
  }

  property("$ne") = forAll { (fieldName: String, fieldValue: List[MyClass]) =>
    val field = new ArrayField[List[MyClass], MyClass, MyClassMetadata, BSONDocument](fieldName)
    (field $ne fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$ne" -> fieldValue))
  }

  property("$in") = forAll { (fieldName: String, fieldValue: List[MyClass]) =>
    val field = new ArrayField[List[MyClass], MyClass, MyClassMetadata, BSONDocument](fieldName)
    (field $in fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$in" -> fieldValue))
  }

  property("$in") = forAll { (fieldName: String, fieldValue: List[List[MyClass]]) =>
    val field = new ArrayField[List[MyClass], MyClass, MyClassMetadata, BSONDocument](fieldName)
    (field $in fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$in" -> fieldValue))
  }

  property("$nin") = forAll { (fieldName: String, fieldValue: List[MyClass]) =>
    val field = new ArrayField[List[MyClass], MyClass, MyClassMetadata, BSONDocument](fieldName)
    (field $nin fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$nin" -> fieldValue))
  }

  property("$nin") = forAll { (fieldName: String, fieldValue: List[List[MyClass]]) =>
    val field = new ArrayField[List[MyClass], MyClass, MyClassMetadata, BSONDocument](fieldName)
    (field $nin fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$nin" -> fieldValue))
  }

  /**
   * Element expressions
   */

  property("$exists") = forAll { (fieldName: String, exists: Boolean) =>
    val field = new ArrayField[List[MyClass], MyClass, MyClassMetadata, BSONDocument](fieldName)
    (field $exists exists).toBSON == BSONDocument(fieldName -> BSONDocument("$exists" -> BSONBoolean(exists)))
  }

  /**
   * Array expressions
   */

  property("$all") = forAll { (fieldName: String, fieldValue: List[MyClass]) =>
    val field = new ArrayField[List[MyClass], MyClass, MyClassMetadata, BSONDocument](fieldName)
    (field $all fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$all" -> fieldValue))
  }

  property("$elemMatch") = forAll { (fieldName: String, fieldValue: MyClass) =>
    val field = new ArrayField[List[MyClass], MyClass, MyClassMetadata, BSONDocument](fieldName)
    (field $elemMatch { x => x -> fieldValue }).toBSON == BSONDocument(fieldName -> BSONDocument("$elemMatch" -> BSONDocument("$eq" -> fieldValue)))
  }

  property("$size") = forAll { (fieldName: String, size: Int) =>
    val field = new ArrayField[List[MyClass], MyClass, MyClassMetadata, BSONDocument](fieldName)
    (field $size size).toBSON == BSONDocument(fieldName -> BSONDocument("$size" -> BSONInteger(size)))
  }

  property("$size") = forAll { (fieldName: String, size: Long) =>
    val field = new ArrayField[List[MyClass], MyClass, MyClassMetadata, BSONDocument](fieldName)
    (field $size size).toBSON == BSONDocument(fieldName -> BSONDocument("$size" -> BSONLong(size)))
  }

}

