package mongodbapi

import reactivemongo.bson.{BSONArray, BSONValue, BSONDocument}
import scala.concurrent.{ExecutionContext, Future}



case class FindQuery(criteria: BSONDocument, projection: Option[BSONDocument])

trait DocumentBuilder[T, D <: Document[T]] {
  def build[T]: D
}

class QueryGenerator[T, D <: Document[T]](implicit documentBuilder: DocumentBuilder[T, D]) {

  val document = documentBuilder.build

  def find(query: D => Expression): FindQuery = {
    FindQuery(query(document).toBSON, None)
  }

}

class Collection[T, D <: Document[T]](collectionName: String)
                                     (implicit documentBuilder: DocumentBuilder[T, D]) {

  private val queryGenerator = new QueryGenerator[T, D]

  def find(query: D => Expression)(implicit ec: ExecutionContext): Future[List[T]] = {
    queryGenerator.find(query) match {
      case FindQuery(criteria, Some(projection)) => //collection.find(criteria, projection)
      case FindQuery(criteria, None) => // collection.find(criteria)
    }
    ???
  }
}