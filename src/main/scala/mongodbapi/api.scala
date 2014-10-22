

import reactivemongo.bson._

package mongodbapi {

  trait Expression {

    def toBSON: BSONDocument

    // syntactic sugar
    def &&(expr2: Expression) = this match {
      case Expression.$and(e1, e2, eN) => Expression.$and(e1, e2, eN :+ expr2)
      case _ => Expression.$and(this, expr2)
    }

    def ||(expr2: Expression) = this match {
      case Expression.$or(e1, e2, eN) => Expression.$or(e1, e2, eN :+ expr2)
      case _ => Expression.$or(this, expr2)
    }
  }

  trait ComparisonExpressions {

    /**
     * Matches values that are equal to the value specified in the query.
     */
    protected abstract class $eqBase[T, B <: BSONValue](field: BaseField with TypeMetadata[T, B], value: BSONValue) extends Expression {
      lazy val toBSON: BSONDocument = BSONDocument(field.fieldName -> value)
    }

    case class ->[T, B <: BSONValue](field: BaseField with TypeMetadata[T, B], value: BSONValue) extends $eqBase[T, B](field, value)

    case class $eq[T, B <: BSONValue](field: BaseField with TypeMetadata[T, B], value: BSONValue) extends $eqBase[T, B](field, value)

    /**
     * $ne Matches all values that are not equal to the value specified in the query.
     */
    case class $ne[T, B <: BSONValue](field: BaseField with TypeMetadata[T, B], value: BSONValue) extends Expression {
      lazy val toBSON = BSONDocument(field.fieldName -> BSONDocument("$ne" -> value))
    }

    /**
     * $gt Matches values that are greater than the value specified in the query.
     */
    case class $gt[T, B <: BSONValue](field: BaseField with TypeMetadata[T, B], value: BSONValue) extends Expression {
      lazy val toBSON = BSONDocument(field.fieldName -> BSONDocument("$gt" -> value))
    }

    /**
     * $gte	Matches values that are greater than or equal to the value specified in the query.
     */
    case class $gte[T, B <: BSONValue](field: BaseField with TypeMetadata[T, B], value: BSONValue) extends Expression {
      lazy val toBSON = BSONDocument(field.fieldName -> BSONDocument("$gte" -> value))
    }

    /**
     * $lt Matches values that are less than the value specified in the query.
     */
    case class $lt[T, B <: BSONValue](field: BaseField with TypeMetadata[T, B], value: BSONValue) extends Expression {
      lazy val toBSON = BSONDocument(field.fieldName -> BSONDocument("$lt" -> value))
    }

    /**
     * $lte	Matches values that are less than or equal to the value specified in the query.
     */
    case class $lte[T, B <: BSONValue](field: BaseField with TypeMetadata[T, B], value: BSONValue) extends Expression {
      lazy val toBSON = BSONDocument(field.fieldName -> BSONDocument("$lte" -> value))
    }

    /**
     * $in Matches any of the values that exist in an array specified in the query.
     */
    case class $in[T, B <: BSONValue](field: BaseField with TypeMetadata[T, B], values: Traversable[BSONValue]) extends Expression {
      lazy val toBSON: BSONDocument = BSONDocument(field.fieldName -> BSONDocument("$in" -> values))
    }

    /**
     * $nin Matches values that do not exist in an array specified to the query.
     */
    case class $nin[T, B <: BSONValue](field: BaseField with TypeMetadata[T, B], values: Traversable[BSONValue]) extends Expression {
      lazy val toBSON: BSONDocument = BSONDocument(field.fieldName -> BSONDocument("$nin" -> values))
    }

  }

  trait LogicalExpressions {

    /**
     * $and Joins query clauses with a logical AND returns all documents that match the conditions of both clauses.
     */
    case class $and(expr1: Expression, expr2: Expression, exprN: Seq[Expression] = Seq()) extends Expression {
      lazy val exprs: Seq[Expression] = Seq(expr1, expr2) ++ exprN
      lazy val toBSON: BSONDocument = BSONDocument("$and" -> BSONArray(exprs.map(_.toBSON)))
    }

    /**
     * $or Joins query clauses with a logical OR returns all documents that match the conditions of either clause.
     */
    case class $or(expr1: Expression, expr2: Expression, exprN: Seq[Expression] = Seq()) extends Expression {
      lazy val exprs: Seq[Expression] = Seq(expr1, expr2) ++ exprN
      lazy val toBSON: BSONDocument = BSONDocument("$or" -> BSONArray(exprs.map(_.toBSON)))
    }

    /**
     * $nor	Joins query clauses with a logical NOR returns all documents that fail to match both clauses.
     */
    case class $nor(expr1: Expression, exprN: Seq[Expression] = Seq()) extends Expression {
      lazy val exprs: Seq[Expression] = expr1 +: exprN
      lazy val toBSON: BSONDocument = BSONDocument("$nor" -> BSONArray(exprs.map(_.toBSON)))
    }

    /**
     * $not Inverts the effect of a query expression and returns documents that do not match the query expression.
     */

    // TODO http://docs.mongodb.org/manual/reference/operator/query/not/

  }

  trait ElementExpressions {

    /**
     * $exists Matches documents that have the specified field.
     */
    case class $exists(field: BaseField, exists: BSONBoolean) extends Expression {
      lazy val toBSON: BSONDocument = BSONDocument(field.fieldName -> BSONDocument("$exists" -> exists))
    }

    /**
     * $type Selects documents if a field is of the specified type.
     */

    // We will never support this expression.

  }

  trait EvaluationExpressions {

    /**
     * $mod Performs a modulo operation on the value of a field and selects documents with a specified result.
     */
    case class $mod(field: BaseField, divisor: BSONValue, remainder: BSONValue) extends Expression {
      lazy val toBSON: BSONDocument = BSONDocument(field.fieldName -> BSONDocument("$mod" -> BSONArray(divisor, remainder)))
    }

    /**
     * $regex Selects documents where values match a specified regular expression.
     */

    // TODO http://docs.mongodb.org/manual/reference/operator/query/regex/

    /**
     * $text Performs text search.
     */

    // TODO http://docs.mongodb.org/manual/reference/operator/query/text/

    /**
     * $where Matches documents that satisfy a JavaScript expression.
     */

    // TODO http://docs.mongodb.org/manual/reference/operator/query/where/

  }

  trait GeospatialExpressions {

    // TODO $geoIntersects Selects geometries that intersect with a GeoJSON geometry.
    // TODO $geoWithin Selects geometries within a bounding GeoJSON geometry.
    // TODO $nearSphere Returns geospatial objects in proximity to a point on a sphere.
    // TODO $near Returns geospatial objects in proximity to a point.

  }

  trait ArrayExpressions {

    /**
     * $all Matches arrays that contain all elements specified in the query.
     */
    case class $all[T, B <: BSONValue](field: BaseField with TypeMetadata[T, B], value: BSONArray) extends Expression {
      lazy val toBSON: BSONDocument = BSONDocument(field.fieldName -> BSONDocument("$all" -> value))
    }

    /**
     * $elemMatch Selects documents if element in the array field matches all the specified $elemMatch condition.
     */
    case class $elemMatch[T, B <: BSONValue](field: BaseField with TypeMetadata[T, B], expr: Expression) extends Expression {
      lazy val toBSON: BSONDocument = BSONDocument(field.fieldName -> BSONDocument("$elemMatch" -> expr.toBSON))
    }

    /**
     * $size Selects documents if the array field is a specified size.
     */
    case class $size(field: BaseField, size: BSONValue) extends Expression {
      lazy val toBSON: BSONDocument = BSONDocument(field.fieldName -> BSONDocument("$size" -> size))
    }

  }

  object Expression
    extends ComparisonExpressions
    with ElementExpressions
    with LogicalExpressions
    with EvaluationExpressions
    with GeospatialExpressions
    with ArrayExpressions


  sealed trait TypeMetadata[T, B <: BSONValue]

  trait DocumentTypeMetadata[T] extends TypeMetadata[T, BSONDocument]

  sealed trait NativeTypeMetadata[T, B <: BSONValue] extends TypeMetadata[T, B]

  sealed trait BaseField {
    self: TypeMetadata[_, _] =>
    private[mongodbapi] val fieldName: String
  }

  class Field[T, B <: BSONValue](private[mongodbapi] val fieldName: String) extends TypeMetadata[T, B] with BaseField

  class ArrayField[C <: Traversable[T], T, B <: BSONValue, TEmbedded <: TypeMetadata[T, B]](private[mongodbapi] val fieldName: String) extends TypeMetadata[T, B] with BaseField {
    self: TEmbedded =>
  }


  trait BSONObjectIDTypeMetadata extends NativeTypeMetadata[BSONObjectID, BSONObjectID]
  trait StringTypeMetadata extends NativeTypeMetadata[String, BSONString]
  trait IntTypeMetadata extends NativeTypeMetadata[Int, BSONInteger]
  trait LongTypeMetadata extends NativeTypeMetadata[Long, BSONLong]
  trait DoubleTypeMetadata extends NativeTypeMetadata[Double, BSONDouble]
  trait BooleanTypeMetadata extends NativeTypeMetadata[Boolean, BSONBoolean]

}

package object mongodbapi {

  implicit class NativeTypeMetadataQuery[T, B <: BSONValue](field: NativeTypeMetadata[T, B])(implicit writer: BSONWriter[T, B]) {
    def ->(value: T): Expression = new Expression {
      def toBSON: BSONDocument = BSONDocument(Seq("$eq" -> writer.write(value)))
    }
    def $eq(value: T): Expression = new Expression {
      def toBSON: BSONDocument = BSONDocument(Seq("$eq" -> writer.write(value)))
    }
    def $in(values: Traversable[T]): Expression = new Expression {
      def toBSON: BSONDocument = BSONDocument("$in" -> BSONArray(values.map(writer.write)))
    }
    def $in(value: T, values: T*): Expression = $in(value +: values)
  }

  implicit class FieldQuery[T, B <: BSONValue](field: Field[T, B])(implicit writer: BSONWriter[T, B]) {

    def ->(value: T): Expression = Expression.$eq(field, writer.write(value))
    def $eq(value: T): Expression = Expression.$eq(field, writer.write(value))
    def $ne(value: T): Expression = Expression.$ne(field, writer.write(value))

    def $gt(value: T): Expression = Expression.$gt(field, writer.write(value))
    def $gte(value: T): Expression = Expression.$gte(field, writer.write(value))

    def $lt(value: T): Expression = Expression.$lt(field, writer.write(value))
    def $lte(value: T): Expression = Expression.$lte(field, writer.write(value))

    def $in(values: Traversable[T]): Expression = Expression.$in(field , values.map(writer.write))
    def $in(value: T, values: T*): Expression = $in(value +: values)

    def $nin(values: Traversable[T]): Expression = Expression.$nin(field , values.map(writer.write))
    def $nin(value: T, values: T*): Expression = $in(value +: values)

    def $exists(exists: Boolean): Expression = Expression.$exists(field, BSONBoolean(exists))

    def $mod(divisor: Int, remainder: Int): Expression = Expression.$mod(field, BSONInteger(divisor), BSONInteger(remainder))
    def $mod(divisor: Long, remainder: Long): Expression = Expression.$mod(field, BSONLong(divisor), BSONLong(remainder))
    def $mod(divisor: Double, remainder: Double): Expression = Expression.$mod(field, BSONDouble(divisor), BSONDouble(remainder))
    def $mod(divisor: Float, remainder: Float): Expression = $mod(divisor.toDouble, remainder.toDouble)

  }

  implicit class ArrayFieldQuery[C <: Traversable[T], T, B <: BSONValue, TEmbedded <: TypeMetadata[T, B]](field: ArrayField[C, T, B, TEmbedded] with TEmbedded)(implicit writer: BSONWriter[T, B]) {

    private def writeArray(arr: C) = BSONArray(arr.map(writer.write))

    def ->(value: C): Expression = Expression.$eq(field, writeArray(value))
    def $eq(value: C): Expression = Expression.$eq(field, writeArray(value))

    def $in(values: Traversable[C]): Expression = Expression.$in(field, values.map(writeArray))
    def $in(value: C, values: C*): Expression = $in(value +: values)

    def $nin(values: Traversable[C]): Expression = Expression.$nin(field, values.map(writeArray))
    def $nin(value: C, values: C*): Expression = $nin(value +: values)

    def $ne(value: T): Expression = Expression.$ne(field, writer.write(value))
    def $ne(value: C): Expression = Expression.$ne(field, writeArray(value))

    def $exists(exists: Boolean): Expression = Expression.$exists(field, BSONBoolean(exists))

    def $all(value: C): Expression = Expression.$all(field, writeArray(value))

    def $elemMatch(query: TEmbedded => Expression): Expression = Expression.$elemMatch(field, query(field))

    def $size(size: Int): Expression = Expression.$size(field, BSONInteger(size))
    def $size(size: Long): Expression = Expression.$size(field, BSONLong(size))

  }

}
