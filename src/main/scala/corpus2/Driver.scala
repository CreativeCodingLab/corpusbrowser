package corpus2

import scala.util.parsing.combinator._
import scala.util.matching.Regex
import scala.xml._
import scala.collection.mutable._
import scala.util.control.Breaks._
import java.io.File
import corpus2.SentenceQuerier._
import corpus2.MICASESentenceParser._
import corpus2.SentenceParser._
import CorpusType._

object Driver {

  val ROOT_CORPUS_DIR = "./corpora/"

  val corpora = new ListBuffer[Corpus]
  val documents = new ListBuffer[Document]
  val sentences = new ListBuffer[Sentence]
  val words = new ListBuffer[Word] //this will not be used anymore
  val tokens = new ListBuffer[Token]
  val c2t = Map.empty[String, Set[Token]];
  


  val w2s = Map.empty[Word, ListBuffer[SentenceIdx]];
  val c2w = Map.empty[String, Set[Word]];
  val l2w = Map.empty[String, Set[Word]];
  val sis = new ListBuffer[SentenceIdx](); //ListBuffer.empty; //needed?

  
  def printDataStructures = {
    println("\nCORPORA\n")
    corpora foreach println

    println("\nDOCUMENTS\n")
    documents foreach println

    println("\nSENTENCES\n")
   // sentences foreach println

    println("\nWORDS\n")
   // words foreach println
   // findWordsThatHaveMultiplePOS
   // c2w foreach println

   println("\nTOKENS\n")
    tokens foreach println
   
  }

  def loadBNCTest = {
    //loadDocumentsFromXML("test.xml")
    val bncCorpus = Corpus(0, "BNCBaby", CorpusType.BNC)
    corpora += bncCorpus
    loadDocumentsFromXML(bncCorpus, "BNCBaby/aca/alp.xml", "BNCBaby/aca/a6u.xml") //, "BNCBaby/aca/acj.xml")
    println("DONE LOADING")
    //installMaps
    println("DONE INSTALLING MAPS")
    printDataStructures
  }

  def loadMICASETest = {
    val micaseCorpus = Corpus(0, "MICASE_dir/", CorpusType.MICASE)
    loadMICASEDocumentsFromXML("MICASE_test.xml", "MI2test.xml")
    //loadDocumentsFromXML("BNCBaby/aca/alp.xml") //, "BNCBaby/aca/a6u.xml", "BNCBaby/aca/acj.xml")
    println("DONE LOADING")
    installMaps
    println("DONE INSTALLING MAPS")
    printDataStructures
  }

  def loadCorpusTest = loadCorpus(CorpusType.BNC, "BNCBaby/")

  def main(args: Array[String]): Unit = {

    //loadMICASETest
    loadBNCTest

    //CasbahHandler.saveToMongo();
    //CasbahHandler.loadFromMongo()

    //testQueries
  }

  def testQueries = {

    //****** TEST QUERIES

    for (f ← findCollocationsWithinRange("happy", 10)) println(f._1.word, f._2) //foreach (m => println(m._2 + " : " +  m._1.word)) 

    for (w ← findWordsThatHaveMultiplePOS) println(w.word + " used " + w.sentences.size + " times as a " + w.pos)
    //for (f <- findCollocations("the")) println (f._1.word, f._2) //foreach (m => println(m._2 + " : " +  m._1.word)) 
    //findSentencesWithClean("this") foreach println

    // val LB = new ListBuffer[SentenceIdx]()
    //findSentenceIdxsWithClean("this"))
    // LB ++= findSentenceIdxsWithLemma("chapter")
    // LB foreach println
    //LB ++= findSentenceIdxsWithClean("chapter")
    // makeCleanWindowForSentenceIdxs(LB, 20) foreach println

    // findRawWindow("the", 30) foreach println
    // findCleanWindow("the", 30) foreach println

    // sentences foreach(s=>println(s.clean))
    // //val re = "(?i)(T.*s)".r //greedy
    // val re = "(?i)(T.*?s)".r //reluctant
    // val exprIdxs = findCleanExpressionIdxsWithExpression(re) 
    // exprIdxs foreach (e=>println(e))

    // makeCleanWindowForExpressionIdxs(exprIdxs, 40) foreach println

    //findSentenceWithRegexOnClean(re) foreach println
    //findRawWindow(re, 30) foreach println
    // 
    //     val check = List("1hello", "2helo", "3hellll")
    // 
    //     val re = ".*(?i)hell.*".r
    // 
    //     val wha = for (
    //       h <- check;
    //       if (re.findFirstMatchIn(h) match {
    //         case Some(s) => true
    //         case None => false
    //       })
    //       )
    //     yield h
    //     
    //     println("wha = " + wha + ", class = " + (wha getClass))

    /*
    CasbahHandler.queryCleanWithRegex("^This.*");
    println("\n\n***\n\n");
    CasbahHandler.queryCleanWithRegex(".*illness.*");
    println("\n\n***\n\n");
    CasbahHandler.queryCleanWithRegex(".*because.*");
    println("\n\n***\n\n");
    CasbahHandler.queryCleanWithRegex(".*therefore.*");
    println("\n\n***\n\n");

    println("done!!! yeah that's right!!")
    */

    //  println("word? " + c2w("the"))

    // findWindowClean("hit", 25) foreach println
    // findWindowRaw("hit", 25) foreach println

    /*
    findSentenceIdxsWithClean("peace").foreach(si => {
      println(si.sentenceId)
      println(si.wordIdx)

      println(words(sentences(si.sentenceId).words(si.wordIdx)).word)

      println(sentences(si.sentenceId).clean)
      // println(sentences(si.sentenceId).words(si.wordIdx))
    })

    findWordsThatHaveMultiplePOS
    */
    // println("SOBERING")
    // findWordsWithClean("sobering").foreach(println)
    //findWindowClean("the", 10).foreach(println)

    //    println("\n\n***\n\nPrinting Words\n\n***\n\n")
    //    w2s.foreach(println)
    //
    //    for ( (key, value) <- w2s)
    //    {
    //    //  println(key.clean + ": in " + value.size + " sentences")
    //    }

    /*
     val map = buildMaps(sentences, List(getClean(_), getPOS(_), getLemma(_)))


     for (m <- map)
     {
     println("\n\n\n*****\n\n\n")
     m.foreach(println)
     }
     */

    //    val cleans = buildMap(sentences, getClean(_));
    //    val lemmas = buildMap(sentences, getLemma(_));
    //
    //    cleans.foreach(println)
    //    lemmas.foreach(println)

    println("DONE parsing...")

  }
}
