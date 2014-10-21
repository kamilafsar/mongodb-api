package mongodbapi

import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification
import reactivemongo.bson.{BSONArray, BSONValue, BSONDocument}

trait BSONDocumentMatchers {
  self: Specification =>

  def asMap: PartialFunction[BSONValue,Map[String, Any]] = {
    case doc: BSONDocument =>
      doc.elements.map {
        case (key, value: BSONDocument) => key -> asMap(value)
        case (key, value: BSONArray) => key -> Seq(value.values.map {
          case arrVal: BSONDocument => asMap(arrVal)
          case arrVal => arrVal
        })
        case element => element
      }.toMap
    case x => throw new Exception(s"asMap: conversion not supported on ${x.getClass.getName}")
  }

  /**
   * BSONDocumenten zijn 'ordered'. In mongo is { aap: 1, noot: 1 } niet gelijk aan { noot: 1, aap: 1}.
   * Met reactivemongo heb je daar heel weinig last van door het (de)serializen van de BSONHandlers.
   *
   * Maar bij unittests willen we dit verschil soms niet zien. Een Map[String, ...] is niet ordered waardoor
   * de equals method conceptueel logischer werkt.
   *
   * TODO: this will only work for BSONDocuments, not BSONArray with objects within it
   */
  def bsonEqualTo(doc2: BSONDocument): Matcher[BSONDocument] = {
    ((doc1: BSONDocument) => asMap(doc1)) ^^ beEqualTo(asMap(doc2))
  }

}
