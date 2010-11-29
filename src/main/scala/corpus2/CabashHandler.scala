package corpus2

import com.novus.casbah.mongodb.MongoConnection
import com.novus.casbah.mongodb.Imports._
import scala.collection.JavaConversions._
import scala.io.Source
import scala.util.parsing.combinator._
import scala.util.matching.Regex
import scala.xml._
import scala.collection.mutable._
import scala.util.control.Breaks._
import java.io.File
import java.util.regex.Pattern._
import java.util.regex._
import corpus2.SentenceParser.{ installMaps }
import corpus2.Driver._

object CasbahHandler {

  // initialize database
  val mongoConn = MongoConnection();
  val mongoDB = mongoConn("corpus");
  val collSentences = mongoDB("sentences")
  val collWords = mongoDB("words")
  val collSentenceIdxs = mongoDB("sentenceIdxs")
  val * = MongoDBObject.empty

  def loadFromMongo(): Unit = {

    collSentences.find sort (MongoDBObject({ "sentenceId" -> 1 })) foreach (dbo ⇒ loadSentence(dbo))
    collWords.find sort (MongoDBObject({ "wordId" -> 1 })) foreach (dbo ⇒ loadWord(dbo))
    installMaps

    def loadSentence(dbo: MongoDBObject) = sentences += Sentence(dbo)
    def loadWord(dbo: MongoDBObject) = words += Word(dbo)
  }

  def saveToMongo(): Unit = {

    println("*** DROPPING EXISITNG COLLECTIONS ***");
    dropExistingCollections()

    println("*** SAVING SENTENCES ***");
    sentences foreach (s ⇒ saveSentence(s))
    println("*** DONE SAVING SENTENCES ***");

    println("*** SAVING WORDS ***");
    words foreach (w ⇒ saveWord(w))
    println("*** DONE SAVING WORDS ***");

    println("*** ENSURE INDEXES ***")
    indexCollections()
    println("*** DONE ENSURE INDEXES ***")

    def dropExistingCollections() = {
      collWords.drop()
      collSentences.drop()
      collSentenceIdxs.drop()
    }

    def saveWord(w: Word) {
      collWords += w.toMongoDBObject()
      for (si ← w.sentences) collSentenceIdxs += si.toMongoDBObject(w.wordId)
    }

    def saveSentence(s: Sentence) = collSentences += s.toMongoDBObject()

    def indexCollections() = {
      collWords.ensureIndex(MongoDBObject("wordId" -> 1), "words_wordId_index", true)
      collSentences.ensureIndex(MongoDBObject("sentenceId" -> 1), "sentences_sentenceId_index", true)
      collSentenceIdxs.ensureIndex(MongoDBObject("wordId" -> 1), "sentenceIdxs_wordId_index", false)
    }
  }

  //val push = $pushAll ("stuff" -> LB) 
  //coll.update(dbo, push)
  //coll += dbo

  def queryCleanWithRegex(s: String) {
    queryCleanWithRegex(s, true)
  }

  def queryCleanWithRegex(s: String, caseInsensitive: Boolean) {
    if (caseInsensitive)
      queryCleanWithRegex(Pattern.compile(s, CASE_INSENSITIVE));

    else
      queryCleanWithRegex(Pattern.compile(s));
  }

  def queryCleanWithRegex(p: Pattern) {
    val mongoColl = mongoDB("bar")
    val cursor = mongoColl.find(MongoDBObject({ "clean" -> p }))

    println("cursor size = " + cursor.size)

    for (v ← cursor) println("s: " + v.get("clean") + "\n")
  }

  def testQuery(): Unit =
    {
      val mongoColl = mongoDB("bar")

      // val q  = MongoDBObject.empty
      // val fields = MongoDBObject("clean" -> 1)
      // for (x <- mongoColl.find(q, fields)) println(x)

      // val cursor = mongoColl.find(*, MongoDBObject{"clean" -> 1})
      // val cursor = mongoColl.find( MongoDBObject({"clean" -> pattern}) )

      // println("cursor = " + cursor)
      // for (v <- cursor) println(v.get("clean") + "\n")
      //    
      //cursor.foreach(print)

      // for {x <- mongoColl} print(x) ; //yield x

    }

}

