package mongodbapi

import reactivemongo.bson._

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

trait Document[T]

class Field[T, B <: BSONValue]
  (private[mongodbapi] val fieldName: String,
   private[mongodbapi] val fieldAncestors: Option[String])
  (implicit writer: BSONWriter[T, B]) {

  def $eq(value: T) = Expression.$eq(this, writer.write(value))
  def $in(values: Iterable[T]) = Expression.$in(this, values.map(writer.write))
}

abstract class ArrayField[C <: Traversable[T], TDoc <: Field[T, B], T, B <: BSONValue]
  (fieldName: String, fieldAncestors: Option[String], embeddedPropertyDocument: TDoc)
  (implicit writer: BSONWriter[T, B])
  extends Field[C, BSONArray](fieldName, fieldAncestors)(ArrayField.writer[C, T, B]) {
  def $elemMatch(query: TDoc => Expression) = Expression.$elemMatch(this, query(embeddedPropertyDocument))
}

object ArrayField {
  def writer[C <: Traversable[T], T, B <: BSONValue](implicit writer: BSONWriter[T, B]): BSONWriter[C, BSONArray] = {
    new BSONWriter[C, BSONArray] {
      def write(t: C): BSONArray = BSONArray(t.map(writer.write))
    }
  }
}

abstract class DocumentField[T]
  (fieldName: String, fieldAncestors: Option[String])
  (implicit writer: BSONWriter[T, BSONDocument])
  extends Field[T, BSONDocument](fieldName, fieldAncestors)
     with Document[T] {

  protected val subfieldAncestors: Some[String] = {
    Some(s"""${fieldAncestors map { _ + "." } getOrElse ""}$fieldName""")
  }

}
