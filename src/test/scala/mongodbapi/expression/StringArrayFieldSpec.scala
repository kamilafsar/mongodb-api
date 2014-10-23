package mongodbapi.expression

import org.scalacheck.Prop._
import org.scalacheck.Properties
import reactivemongo.bson._

object StringArrayFieldSpec extends Properties("ArrayField[List[String], String, TypeMetadata[String, BSONString], BSONString]") {

  import mongodbapi._

  /**
   * Comparison expressions
   */

  property("$eq") = forAll { (fieldName: String, fieldValue: String) =>
    val field = new ArrayField[List[String], String, StringArrayElementTypeMetadata, BSONString](fieldName)
    (field $eq fieldValue).toBSON == BSONDocument(fieldName -> BSONString(fieldValue))
  }

  property("$eq") = forAll { (fieldName: String, fieldValue: List[String]) =>
    val field = new ArrayField[List[String], String, StringArrayElementTypeMetadata, BSONString](fieldName)
    (field $eq fieldValue).toBSON == BSONDocument(fieldName -> BSONArray(fieldValue.map(BSONString)))
  }

  property("->") = forAll { (fieldName: String, fieldValue: String) =>
    val field = new ArrayField[List[String], String, StringArrayElementTypeMetadata, BSONString](fieldName)
    (field -> fieldValue).toBSON == BSONDocument(fieldName -> BSONString(fieldValue))
  }

  property("->") = forAll { (fieldName: String, fieldValue: List[String]) =>
    val field = new ArrayField[List[String], String, StringArrayElementTypeMetadata, BSONString](fieldName)
    (field -> fieldValue).toBSON == BSONDocument(fieldName -> BSONArray(fieldValue.map(BSONString)))
  }

  property("$ne") = forAll { (fieldName: String, fieldValue: String) =>
    val field = new ArrayField[List[String], String, StringArrayElementTypeMetadata, BSONString](fieldName)
    (field $ne fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$ne" -> BSONString(fieldValue)))
  }

  property("$ne") = forAll { (fieldName: String, fieldValue: List[String]) =>
    val field = new ArrayField[List[String], String, StringArrayElementTypeMetadata, BSONString](fieldName)
    (field $ne fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$ne" -> BSONArray(fieldValue.map(BSONString))))
  }

  property("$in") = forAll { (fieldName: String, fieldValue: List[String]) =>
    val field = new ArrayField[List[String], String, StringArrayElementTypeMetadata, BSONString](fieldName)
    (field $in fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$in" -> BSONArray(fieldValue.map(BSONString))))
  }

  property("$in") = forAll { (fieldName: String, fieldValue: List[List[String]]) =>
    val field = new ArrayField[List[String], String, StringArrayElementTypeMetadata, BSONString](fieldName)
    (field $in fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$in" -> BSONArray(fieldValue.map(v => BSONArray(v.map(BSONString))))))
  }

  property("$nin") = forAll { (fieldName: String, fieldValue: List[String]) =>
    val field = new ArrayField[List[String], String, StringArrayElementTypeMetadata, BSONString](fieldName)
    (field $nin fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$nin" -> BSONArray(fieldValue.map(BSONString))))
  }

  property("$nin") = forAll { (fieldName: String, fieldValue: List[List[String]]) =>
    val field = new ArrayField[List[String], String, StringArrayElementTypeMetadata, BSONString](fieldName)
    (field $nin fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$nin" -> BSONArray(fieldValue.map(v => BSONArray(v.map(BSONString))))))
  }

  /**
   * Element expressions
   */

  property("$exists") = forAll { (fieldName: String, exists: Boolean) =>
    val field = new ArrayField[List[String], String, StringArrayElementTypeMetadata, BSONString](fieldName)
    (field $exists exists).toBSON == BSONDocument(fieldName -> BSONDocument("$exists" -> BSONBoolean(exists)))
  }

  /**
   * Array expressions
   */

  property("$all") = forAll { (fieldName: String, fieldValue: List[String]) =>
    val field = new ArrayField[List[String], String, StringArrayElementTypeMetadata, BSONString](fieldName)
    (field $all fieldValue).toBSON == BSONDocument(fieldName -> BSONDocument("$all" -> BSONArray(fieldValue.map(BSONString))))
  }

  property("$elemMatch") = forAll { (fieldName: String, fieldValue: String) =>
    val field = new ArrayField[List[String], String, StringArrayElementTypeMetadata, BSONString](fieldName)
    (field $elemMatch { _ -> fieldValue }).toBSON == BSONDocument(fieldName -> BSONDocument("$elemMatch" -> BSONDocument("$eq" -> BSONString(fieldValue))))
  }

  property("$size") = forAll { (fieldName: String, size: Int) =>
    val field = new ArrayField[List[String], String, StringArrayElementTypeMetadata, BSONString](fieldName)
    (field $size size).toBSON == BSONDocument(fieldName -> BSONDocument("$size" -> BSONInteger(size)))
  }

  property("$size") = forAll { (fieldName: String, size: Long) =>
    val field = new ArrayField[List[String], String, StringArrayElementTypeMetadata, BSONString](fieldName)
    (field $size size).toBSON == BSONDocument(fieldName -> BSONDocument("$size" -> BSONLong(size)))
  }

}

