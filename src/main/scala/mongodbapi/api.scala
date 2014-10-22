

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

  object Expression {

    case class $eq[T, B <: BSONValue](field: BaseField with TypeMetadata[T, B], value: BSONValue) extends Expression {
      lazy val toBSON: BSONDocument = BSONDocument(field.fieldName -> value)
    }

    case class $in[T, B <: BSONValue](field: BaseField with TypeMetadata[T, B], values: Traversable[BSONValue]) extends Expression {
      lazy val toBSON: BSONDocument = BSONDocument(field.fieldName -> BSONDocument("$in" -> values))
    }

    case class $elemMatch[T, B <: BSONValue](field: BaseField with TypeMetadata[T, B], expr: Expression) extends Expression {
      lazy val toBSON: BSONDocument = BSONDocument(field.fieldName -> BSONDocument("$elemMatch" -> expr.toBSON))
    }

    case class $and(expr1: Expression, expr2: Expression, exprN: Seq[Expression] = Seq()) extends Expression {
      lazy val exprs: Seq[Expression] = Seq(expr1, expr2) ++ exprN
      lazy val toBSON: BSONDocument = BSONDocument("$and" -> BSONArray(exprs.map(_.toBSON)))
    }

    case class $or(expr1: Expression, expr2: Expression, exprN: Seq[Expression] = Seq()) extends Expression {
      lazy val exprs: Seq[Expression] = Seq(expr1, expr2) ++ exprN
      lazy val toBSON: BSONDocument = BSONDocument("$or" -> BSONArray(exprs.map(_.toBSON)))
    }

  }

  trait TypeMetadata[T, B <: BSONValue]

  trait NativeTypeMetadata[T, B <: BSONValue] extends TypeMetadata[T, B]

  trait BaseField {
    self: TypeMetadata[_, _] =>
    private[mongodbapi] val fieldName: String
  }

  class Field[T, B <: BSONValue](private[mongodbapi] val fieldName: String) extends TypeMetadata[T, B] with BaseField

  class ArrayField[C <: Traversable[T], T, B <: BSONValue, TEmbedded <: TypeMetadata[T, B]](private[mongodbapi] val fieldName: String) extends TypeMetadata[T, B] with BaseField {
    self: TEmbedded =>
  }


  trait IntTypeMetadata extends NativeTypeMetadata[Int, BSONInteger]
  trait StringTypeMetadata extends NativeTypeMetadata[String, BSONString]


}

package object mongodbapi {

  implicit class NativeTypeMetadataQuery[T, B <: BSONValue](field: NativeTypeMetadata[T, B])(implicit writer: BSONWriter[T, B]) {
    def $eq(value: T): Expression = new Expression {
      def toBSON: BSONDocument = BSONDocument("$eq" -> value)
    }
    def $in(values: Traversable[T]): Expression = new Expression {
      def toBSON: BSONDocument = BSONDocument("$in" -> values)
    }
    def $in(values: T*): Expression = $in(values)
  }

  implicit class FieldQuery[T, B <: BSONValue](field: Field[T, B])(implicit writer: BSONWriter[T, B]) {
    def $eq(value: T): Expression = Expression.$eq(field, writer.write(value))
    def $in(values: Traversable[T]): Expression = Expression.$in(field , values.map(writer.write))
    def $in(values: T*): Expression = $in(values)
  }

  implicit class ArrayFieldQuery[C <: Traversable[T], T, B <: BSONValue, TEmbedded <: TypeMetadata[T, B]](field: ArrayField[C, T, B, TEmbedded] with TEmbedded)(implicit writer: BSONWriter[T, B]) {
    def $eq(value: C): Expression = Expression.$eq(field, BSONArray(value.map(writer.write)))
    def $in(values: Traversable[Traversable[T]]): Expression = Expression.$in(field, values.map(v => BSONArray(v.map(writer.write))))
    def $in(values: Traversable[T]*): Expression = $in(values)

    def $elemMatch(query: TEmbedded => Expression) = Expression.$elemMatch(field, query(field))
  }

}
