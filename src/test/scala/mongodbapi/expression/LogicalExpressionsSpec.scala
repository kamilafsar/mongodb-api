package mongodbapi.expression

import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Properties}
import reactivemongo.bson._

object LogicalExpressionsSpec extends Properties("LogicalExpressions") {

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

  property("$and") = forAll { (fieldName1: String, fieldValue1: String,
                               fieldName2: String, fieldValue2: MyClass,
                               fieldName3: String, fieldValue3: Int) =>
    val field1 = new Field[String, BSONString](fieldName1, None)
    val field2 = new Field[MyClass, BSONDocument](fieldName2, None)
    val field3 = new Field[Int, BSONInteger](fieldName3, None)

    val expr = ((field1 $eq fieldValue1) && (field2 $eq fieldValue2) && (field3 $eq fieldValue3))

    expr.toBSON == BSONDocument(
      "$and" -> BSONArray(
        BSONDocument(fieldName1 -> fieldValue1),
        BSONDocument(fieldName2 -> fieldValue2),
        BSONDocument(fieldName3 -> fieldValue3)
      )
    )
  }

  property("$or") = forAll { (fieldName1: String, fieldValue1: String,
                               fieldName2: String, fieldValue2: MyClass,
                               fieldName3: String, fieldValue3: Int) =>
    val field1 = new Field[String, BSONString](fieldName1, None)
    val field2 = new Field[MyClass, BSONDocument](fieldName2, None)
    val field3 = new Field[Int, BSONInteger](fieldName3, None)

    val expr = ((field1 $eq fieldValue1) || (field2 $eq fieldValue2) || (field3 $eq fieldValue3))

    expr.toBSON == BSONDocument(
      "$or" -> BSONArray(
        BSONDocument(fieldName1 -> fieldValue1),
        BSONDocument(fieldName2 -> fieldValue2),
        BSONDocument(fieldName3 -> fieldValue3)
      )
    )
  }

  // TODO property("$nor")

}
