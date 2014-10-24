package mongodbapi

import reactivemongo.bson.{BSONArray, BSONValue, BSONDocument}
import scala.concurrent.{ExecutionContext, Future}

case class FindQuery(criteria: BSONDocument, projection: Option[BSONDocument])

class QueryGenerator[T] {

  def find[D <: DocumentTypeMetadata[T]](query: D => Expression)(implicit document: D): FindQuery = {
    FindQuery(query(document).toBSON, None)
  }

}
