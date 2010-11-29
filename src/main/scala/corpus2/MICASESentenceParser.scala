package corpus2

import scala.util.parsing.combinator._
import scala.util.matching.Regex
import scala.xml._
import scala.collection.mutable._
import scala.util.control.Breaks._
import java.io.File
import corpus2.SentenceQuerier._
import corpus2.Driver._

object MICASESentenceParser {

  def loadMICASEDocumentsFromXML(xmlFilenames: String*): Unit = {
    println("in loadDocumentsFromXML")
    for (xmlFilename ← xmlFilenames) { println("xmlfile = " + (ROOT_CORPUS_DIR + xmlFilename)); parseXML(XML.loadFile(ROOT_CORPUS_DIR + xmlFilename)) }
  }

  //def extractSentence(doc: Document, s: Node): Sentence = {
  def extractMICASESentences(doc: Document, body: Node): Unit = {

    val partofspeech = "NONE"
    val lemma = "NONE"

    def storeWord(sentenceId: Int, wordIdx: Int, cleanIdx: Int, trim: String) : Int = {

      //val sentenceIdx = SentenceIdx(sentenceId, wordIdx, rawIdx + idxOf, cleanIdx)
      val sentenceIdx = SentenceIdx(sentenceId, wordIdx, cleanIdx, cleanIdx) //TODO figure out how to deal with rawIdx (ex., should i include <OVERLAP>? etc?)
      
      var wordId = -1;
      var addedToExistingWord = false
  
      //1. check and see if this word already exists in the global word map
      if (c2w.contains(trim)) {
        //ok this means that we have already stored a word that looks like this
        //let's check the set and see if there is one with the same POS
        //2. check to see if this word has the same part of speech as one already added
        for (w ← c2w(trim)) {
          if (w.word == trim && w.pos == partofspeech) {
            //2a. if yes, then add the current sentenceIdx to that existing word
            w.sentences.append(sentenceIdx)
            addedToExistingWord = true
            wordId = w.wordId
          }
        }

        //2b. otherwise, if no, add this new word to the set (ie because it has a different POS)
        if (addedToExistingWord == false) {
          val LB = ListBuffer.empty[SentenceIdx]
          LB.append(sentenceIdx)
          val newWord = Word(words.size, trim, lemma, partofspeech, LB)
          words += newWord
          c2w(trim) += newWord
          wordId = newWord.wordId
        }
      } //3. create a new set of words with this word/pos and add it to the global word map
else {
        val LB = ListBuffer.empty[SentenceIdx]
        LB.append(sentenceIdx)
        val newWord = Word(words.size, trim, lemma, partofspeech, LB)
        words += newWord
        val wordSet = Set(newWord)
        c2w += (trim -> wordSet)
        wordId = newWord.wordId
      }

      wordId
      //4. increment the indexes
      //wordIdx += 1;
      //cleanIdx += clean.length;
      //rawIdx += wordNode.toString.length;

      //wordIds += wordId
    }

    def splitSentence(user: String, overlap: Boolean, node: Text) {

         val rawSentence = node.text.trim
        println("RAW: " + rawSentence)
         val re = "[A-Za-z][\\.\\?\\!] ".r

         val splitList = re.findAllIn(rawSentence).matchData.toList

         if (splitList.size == 0) {storeSentence(user, overlap, rawSentence)}
         else {
           var idx = 0
          for (sl <- splitList) {
           //println(sl)
            storeSentence(user, overlap, rawSentence.substring(idx, sl.start + 2).trim)
           idx = sl.start + 2
          }
          storeSentence(user,overlap, rawSentence.substring(idx, rawSentence.size).trim)
         }

      }

    def storeSentence(user: String, overlap: Boolean, rawSentence: String) {
      val wordIds = new ListBuffer[Int]()
     val sentenceId = sentences.size

      println("RAW SENTENCE: " + rawSentence)

      
      val ssArray = rawSentence.split(' ')
      println("ssArray length = " + ssArray.length)
   
      var cleanIdx = 0
      var wordIdx = 0
      
      for (word <- ssArray) {
        //println("S: " + word)

        val re = "[,.!?_]+".r

        val wha =
          re.findFirstMatchIn(word) match {
            case Some(p) => {
              //val s1 = s.substring(0, p.start)
              //val s2 = s.substring(p.start, s.length)
              wordIds += storeWord(sentenceId, wordIdx, cleanIdx, word.substring(0, p.start))
              wordIdx = wordIdx + 1
              wordIds += storeWord(sentenceId, wordIdx, cleanIdx + p.start, word.substring(p.start, word.length))
              wordIdx = wordIdx + 1
              //println("punc: " + s1 + " : " + s2)
            }
            case None => {
              wordIds += storeWord(sentenceId, wordIdx, cleanIdx, word)
              wordIdx = wordIdx + 1
              
              //println("no punc: " + s)
            }
          }
          cleanIdx += word.length + 1;
      }


      // val sentenceId = sentences.size
       val sentenceNum = "" + sentenceId
      // println("sentenceId: " + sentenceId + ": " + user + ": overlap=" + overlap + ": ")
      //val sentence = Sentence(sentenceId, sentenceNum, wordNodes.mkString, s.text, wordIds)
   
      /* TEMPORARILY COMMENTING OUT WHILE REORGANIZING 
      val sentence = Sentence(sentenceId, sentenceNum, "NONE", rawSentence, wordIds)
      sentences.append(sentence)
      */

    }

    //TODO: should have an option to splitSentence or store the whole thing directly.
    def recursiveMatcher(n: Node, user: String, overlap: Boolean) {
      for (cs ← n.child) {
        for (element ← cs)
          element match {
            case x: Text ⇒ if (x.text.trim.length > 0) splitSentence(user, overlap, x) //storeSentence(user, overlap, x)
            case <U>{ _* }</U> ⇒ recursiveMatcher(element, (element \ "@WHO")(0).text, overlap)
            case <OVERLAP>{ _* }</OVERLAP> ⇒ { recursiveMatcher(element, user, true) }
            case _ ⇒
          }
      }
    }

    def firstMatcher(n: Node) {
      n match {
        case <BODY>{ elements@_* }</BODY> ⇒
          for (element ← elements)
            element match {
              case <U>{ _* }</U> ⇒ recursiveMatcher(element, (element \ "@WHO")(0).text, false)
              case _ ⇒
            }
      }
    }

    firstMatcher(body)

    if (1 == 1) return

    //val number = (s \ "@n")(0).text
    val number = "1";
    val wordNodes = new ListBuffer[Node]()
    val wordIds = new ListBuffer[Int]()

    val sentenceId = sentences.size
    var wordIdx = 0;
    var rawIdx = 0;
    var cleanIdx = 0;
    for (wordNode ← wordNodes) {

      val clean = wordNode.text
      val idxOf = wordNode.mkString.indexOf(clean)
      val sentenceIdx = SentenceIdx(sentenceId, wordIdx, rawIdx + idxOf, cleanIdx)
      //val sentenceIdx = SentenceIdx(sentenceId, wordIdx, idxOf, cleanIdx)
      val partofspeech = (wordNode \ "@type").text.trim
      val lemma = (wordNode \ "@lemma").text.trim
      val trim = clean.trim.toLowerCase

      var wordId = -1;
      var addedToExistingWord = false

      //1. check and see if this word already exists in the global word map
      if (c2w.contains(trim)) {
        //ok this means that we have already stored a word that looks like this
        //let's check the set and see if there is one with the same POS
        //2. check to see if this word has the same part of speech as one already added
        for (w ← c2w(trim)) {
          if (w.word == trim && w.pos == partofspeech) {
            //2a. if yes, then add the current sentenceIdx to that existing word
            w.sentences.append(sentenceIdx)
            addedToExistingWord = true
            wordId = w.wordId
          }
        }

        //2b. otherwise, if no, add this new word to the set (ie because it has a different POS)
        if (addedToExistingWord == false) {
          val LB = ListBuffer.empty[SentenceIdx]
          LB.append(sentenceIdx)
          val newWord = Word(words.size, trim, lemma, partofspeech, LB)
          words += newWord
          c2w(trim) += newWord
          wordId = newWord.wordId
        }
      } //3. create a new set of words with this word/pos and add it to the global word map
else {
        val LB = ListBuffer.empty[SentenceIdx]
        LB.append(sentenceIdx)
        val newWord = Word(words.size, trim, lemma, partofspeech, LB)
        words += newWord
        val wordSet = Set(newWord)
        c2w += (trim -> wordSet)
        wordId = newWord.wordId
      }

      //4. increment the indexes
      wordIdx += 1;
      cleanIdx += clean.length;
      rawIdx += wordNode.toString.length;

      wordIds += wordId
    }

    // println("s mkString = " + s.mkString)
    // println("s text     = " + s.text)
    // println("wordNodes  = " + wordNodes.mkString)

    //val sentence = Sentence(sentenceId, number, s.mkString, s.text, wordIds)
       /*
    var i = 0;
    for (word <- wL) {
      val si = SentenceIdx(sentence, i);
      sis.append(si)
      buildWordMap(si, word)
      i += 1
    }
    */

    //return sentence
  }
  def parseXML(xml: NodeSeq): Unit =
    {
      val micasedoc = ((xml \\ "TEI.2")(0) \ "@ID").text
      println("MICASE FILE = " + micasedoc)
      val title1 = (xml \\ "TITLE")(0).text.trim
      val title2 = (xml \\ "TITLE")(1).text.trim
      println(micasedoc, title1 + "..." + title2)

      val doc = Document(documents.size, micasedoc, title1 + " " + title2, ListBuffer.empty[Int])
      documents += doc

      //for (s ← (xml \\ "BODY")) { extractMICASESentences(doc, s) }
      extractMICASESentences(doc, (xml \\ "BODY")(0))

    }

}

