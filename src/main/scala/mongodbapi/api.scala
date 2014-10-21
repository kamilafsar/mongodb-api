

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

    case class $eq[T, B <: BSONValue](field: Field[T, B], value: BSONValue) extends Expression {
      lazy val toBSON: BSONDocument = BSONDocument(field.fieldName -> value)
    }

    case class $in[T, B <: BSONValue](field: Field[T, B], values: Iterable[BSONValue]) extends Expression {
      lazy val toBSON: BSONDocument = BSONDocument(field.fieldName -> BSONDocument("$in" -> values))
    }

    case class $elemMatch[T, B <: BSONValue](field: Field[T, B], expr: Expression) extends Expression {
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

  trait TypeMetadata[T]

  class Field[T, B <: BSONValue](private[mongodbapi] val fieldName: String) extends TypeMetadata[T]

  class ArrayField[C <: Traversable[T], T, B <: BSONValue, TEmbedded](fieldName: String) extends Field[C, BSONArray](fieldName) {
    self: TEmbedded =>
  }

  class DocumentField[T](fieldName: String) extends Field[T, BSONDocument](fieldName)
}

package object mongodbapi {

  implicit class FieldQuery[T, B <: BSONValue](field: Field[T, B])(implicit writer: BSONWriter[T, B]) {
    def $eq(value: T): Expression = Expression.$eq(field, writer.write(value))
    def $in(values: Iterable[T]): Expression = Expression.$in(field , values.map(writer.write))
    def $in(values: T*): Expression = $in(values)
  }

  implicit class ArrayFieldQuery[C <: Traversable[T], T, B <: BSONValue, TEmbedded](field: ArrayField[C, T, B, TEmbedded] with TEmbedded)(implicit writer: BSONWriter[T, B]) {
    def $elemMatch(query: TEmbedded => Expression) = Expression.$elemMatch(field, query(field))
  }

}

