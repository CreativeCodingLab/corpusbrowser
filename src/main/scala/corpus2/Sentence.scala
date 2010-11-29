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
import CorpusType._

object Sentence {

  def apply(x: MongoDBObject, corpusType: CorpusType): Sentence = {

    val sentenceId = x.getAs[Int]("sentenceId").get
    val number = x.getAs[String]("number").get
    val clean = x.getAs[String]("clean").get
    val words = x.getAs[BasicDBList]("words").get;
    val wordIds = for (w ← words.toArray) yield (w.asInstanceOf[Int])
    val wordBuffer = new ListBuffer[Int]()
    wordBuffer ++= wordIds

    var raw = ""
    if (corpusType == BNC) {
      raw = x.getAs[String]("raw").get
    }

    if (corpusType == MICASE) {
      val speaker = x.getAs[String]("speaker").get
      val overlap = x.getAs[Boolean]("overlap").get
    }

    /* TEMPORARIY COMMENTING OUT */
    //Sentence(sentenceId, number, raw, clean, wordBuffer)
    Sentence(-1, "", "", "", "", new ListBuffer[Int], new ListBuffer[PositionIdx])
  }

  def apply(x: MongoDBObject): Sentence = {
    apply(x, CorpusType.BNC);
  }
}

// TODO need to add variables for MICASE (speaker, overlap)
/*
case class Sentence(sentenceId: Int, number: String, raw: String, clean: String, words: ListBuffer[Int]) {
  override def toString(): String = {
    "#" + sentenceId + ":n=" + number + "\n" + raw + "\n" + clean + "\n" + words;
  }

  def toMongoDBObject(): MongoDBObject = MongoDBObject("sentenceId" -> sentenceId, "number" -> number, "raw" -> raw, "clean" -> clean, "words" -> words)
}
*/

case class Sentence(id: Int, number: String, 
  raw: String, clean: String, partofspeech: String, 
  tokens: ListBuffer[Int], positionIdxs: ListBuffer[PositionIdx]) {
  
  val words = new ListBuffer[Int] //temp, not being used anymore...
  
  override def toString(): String = {
    "#" + id + ":n=" + number + "\n" + raw + "\n" + clean + "\n" + partofspeech + "\n" + words;
  }

  def toMongoDBObject(): MongoDBObject = MongoDBObject("sentenceId" -> id, "number" -> number, "raw" -> raw, "clean" -> clean, "words" -> words)
}

