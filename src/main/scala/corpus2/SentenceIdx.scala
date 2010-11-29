package corpus2

import com.novus.casbah.mongodb.MongoConnection
import com.novus.casbah.mongodb.Imports._

import scala.util.parsing.combinator._
import scala.util.matching.Regex
import scala.xml._
import scala.collection.mutable._
import scala.util.control.Breaks._
import java.io.File
import com.novus.casbah.mongodb.Imports._

object SentenceIdx {
  //we probably don't need this apply(String) method anymore 
  // def apply(s: String): SentenceIdx = {
  //   val ps = s.split(":")
  //   SentenceIdx(ps(0).toInt, ps(1).toInt, ps(2).toInt, ps(3).toInt)
  // }
  //or this toStrings method
  //def toStrings(lb: ListBuffer[SentenceIdx]): ListBuffer[String] = for (si <- lb) yield si.toString

  def apply(x: MongoDBObject): SentenceIdx = {
    //val wordId = x.getAs[Int]("wordId").get
    val sentenceId = x.getAs[Int]("sentenceId").get
    val wordIdx = x.getAs[Int]("wordIdx").get
    val rawIdx = x.getAs[Int]("rawIdx").get
    val cleanIdx = x.getAs[Int]("cleanIdx").get
    SentenceIdx(sentenceId, wordIdx, rawIdx, cleanIdx)
  }
}

case class SentenceIdx(sentenceId: Int, wordIdx: Int, rawIdx: Int, cleanIdx: Int) {
  override def toString(): String = "[" + sentenceId + ":" + wordIdx + ":" + rawIdx + ":" + cleanIdx + "]"

  def toMongoDBObject(wordId: Int): MongoDBObject = MongoDBObject("wordId" -> wordId, "sentenceId" -> sentenceId, "wordIdx" -> wordIdx, "rawIdx" -> rawIdx, "cleanIdx" -> cleanIdx)
}


case class SentenceIdx2(sentenceId: Int, startIdx: Int, endIdx: Int) {
  override def toString(): String = "[" + sentenceId + ":" + startIdx + ":" + endIdx + "]"
}

//position of the token within each of the different sentence modalities
case class PositionIdx(sentenceId: Int, tokenIdx: Int, 
  rawStartIdx:Int, rawEndIdx:Int, 
  cleanStartIdx:Int, cleanEndIdx:Int,    
  posStartIdx:Int, posEndIdx:Int) {

  override def toString(): String = "[" + sentenceId + ":" + tokenIdx + ":"
    + rawStartIdx + ":" + rawEndIdx + ":" 
    + cleanStartIdx + ":" + cleanEndIdx + ":" 
    + posStartIdx + ":" + posEndIdx + "]"

}
