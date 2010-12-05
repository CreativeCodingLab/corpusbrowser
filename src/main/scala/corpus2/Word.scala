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

// Corpus (id, name) --> 
// Document (id, code, title, sentenceIds)

// Sentence (id, number, raw sentence, clean sentence, wordIds) 

// Word (id, word, lemma, pos, sentenceIdx)

object CorpusType extends Enumeration {
  type CorpusType = Value
  val BNC, MICASE = Value
}
import CorpusType._

case class Corpus(corpusId: Int, corpusRootDir: String, corpusType: CorpusType) {
  var documents = new ListBuffer[Int]
  //override def toString(): String = "CorpusRoot = " + corpusRootDir + ", Corpus = " + corpusType
  override def toString(): String = "" + corpusType
}

case class Document(documentId: Int, code: String, title: String, sentences: ListBuffer[Int]) {
  override def toString(): String = code

  def this() = this(1, "", "", new ListBuffer[Int])
}

object Word {
  def apply(x: MongoDBObject): Word = {
    val wordId = x.getAs[Int]("wordId").get
    val word = x.getAs[String]("word").get
    val lemma = x.getAs[String]("lemma").get
    val pos = x.getAs[String]("pos").get

    val sis = new ListBuffer[SentenceIdx] ++= {
      for (si ← CasbahHandler.collSentenceIdxs.find(MongoDBObject({ "wordId" -> wordId }))) yield (SentenceIdx(si))
    }

    Word(wordId, word, lemma, pos, sis)
  }
}
case class Word(wordId: Int, word: String, lemma: String, pos: String, sentences: ListBuffer[SentenceIdx]) {
  def toMongoDBObject(): MongoDBObject = MongoDBObject("wordId" -> wordId, "word" -> word, "lemma" -> lemma, "pos" -> pos)

  override def toString(): String = {
    "id: " + wordId + " | " + "clean: <" + word + "> | " + "pos: <" + pos + "> | " + sentences
  }
}

case class Token(id:Int, raw:String, clean:String, partofspeech:String, lemma:String) {
  override def toString(): String = {
    "id: " + id + " \t| clean: [" + clean + "] | raw: [" + raw  + "] | pos: [" + partofspeech + "] "
  }
}

