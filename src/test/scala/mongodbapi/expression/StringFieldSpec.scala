package mongodbapi.expression

import reactivemongo.bson._

import org.scalacheck.{Properties}
import org.scalacheck.Prop.forAll

object StringFieldSpec extends Properties("Field[String, BSONString]") {

  import mongodbapi._

  protected def fieldWithName(fieldName: String): Field[String, BSONString] = {
    new Field[String, BSONString](fieldName, None)
  }

  /**
   * Comparison expressions
   */

  property("$eq") = forAll { (fieldName: String, fieldValue: String) =>
    val field: Field[String, BSONString] = fieldWithName(fieldName)
    (field $eq fieldValue).toBSON == BSONDocument(fieldName -> BSONString(fieldValue))
  }

  property("->") = forAll { (fieldName: String, fieldValue: String) =>
    val field: Field[String, BSONString] = fieldWithName(fieldName)
    (field -> fieldValue).toBSON == BSONDocument(fieldName -> BSONString(fieldValue))
  }

  property("$ne") = forAll { (fieldName: String, fieldValue: String) =>
    val field: Field[String, BSONString] = fieldWithName(fieldName)
    (field $ne fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$ne" -> BSONString(fieldValue)))
  }

  property("$gt") = forAll { (fieldName: String, fieldValue: String) =>
    val field: Field[String, BSONString] = fieldWithName(fieldName)
    (field $gt fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$gt" -> BSONString(fieldValue)))
  }

  property("$gte") = forAll { (fieldName: String, fieldValue: String) =>
    val field: Field[String, BSONString] = fieldWithName(fieldName)
    (field $gte fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$gte" -> BSONString(fieldValue)))
  }

  property("$lt") = forAll { (fieldName: String, fieldValue: String) =>
    val field: Field[String, BSONString] = fieldWithName(fieldName)
    (field $lt fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$lt" -> BSONString(fieldValue)))
  }

  property("$lte") = forAll { (fieldName: String, fieldValue: String) =>
    val field: Field[String, BSONString] = fieldWithName(fieldName)
    (field $lte fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$lte" -> BSONString(fieldValue)))
  }

  property("$in") = forAll { (fieldName: String, fieldValue: List[String]) =>
    val field: Field[String, BSONString] = fieldWithName(fieldName)
    (field $in fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$in" -> BSONArray(fieldValue.map(BSONString))))
  }

  property("$nin") = forAll { (fieldName: String, fieldValue: List[String]) =>
    val field: Field[String, BSONString] = fieldWithName(fieldName)
    (field $nin fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$nin" -> BSONArray(fieldValue.map(BSONString))))
  }

  /**
   * Element expressions
   */

  property("$exists") = forAll { (fieldName: String, exists: Boolean) =>
    val field: Field[String, BSONString] = fieldWithName(fieldName)
    (field $exists exists).toBSON == BSONDocument(fieldName -> BSONDocument("$exists" -> BSONBoolean(exists)))
  }

  /**
   * Evaluation expressions
   */

  property("$mod") = forAll { (fieldName: String, divisor: Int, remainder: Int) =>
    val field: Field[String, BSONString] = fieldWithName(fieldName)
    (field $mod (divisor, remainder)).toBSON == BSONDocument(fieldName -> BSONDocument("$mod" -> BSONArray(BSONInteger(divisor), BSONInteger(remainder))))
  }


}
