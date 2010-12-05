package corpus2

import scala.util.parsing.combinator._
import scala.util.matching.Regex
import scala.xml._
import scala.collection.mutable._
import scala.util.control.Breaks._
import java.io.File
import corpus2.SentenceQuerier._
import CorpusType._
import corpus2.Driver._

object SentenceParser {
 
 
  def loadCorpora(corpusType: CorpusType, corporaStrs: String*): Unit = {
    corporaStrs.foreach(loadCorpus(corpusType, _))
  }

  def loadCorpus(corpusType: CorpusType, corpusDir: String): Unit = {
    val corpus = Corpus(corpora.size, corpusDir, corpusType)
    corpora += corpus
    val files = CorpusUtils.recursiveListFiles(new File(ROOT_CORPUS_DIR + corpusDir), """.*\.xml$""".r)
    loadDocumentsFromXML(for (f ← files) yield f.toString)

    def loadDocumentsFromXML(xmlFilenames: Array[String]): Unit = {
      for (xmlFilename ← xmlFilenames) {
        println("current filename = " + xmlFilename);
        //val xml = XML.loadFile(xmlFilename)
        //println("loaded xmlfile " + xml)
        parseXML(corpus, XML.loadFile(xmlFilename))
        //parseXML(xml)
      }
    }
  }

  def installMaps() = {
    for (w ← words) {
      c2w(w.word) = if (!c2w.contains(w.word)) { new HashSet[Word] += w } else { c2w(w.word) += w }
      l2w(w.lemma) = if (!l2w.contains(w.lemma)) { new HashSet[Word] += w } else { l2w(w.lemma) += w }
      w2s(w) = if (!w2s.contains(w)) { new ListBuffer[SentenceIdx] ++= w.sentences } else { w2s(w) ++= w.sentences }
    }
  }

  def loadDocumentsFromXML(corpus: Corpus, xmlFilenames: String*): Unit = {
    println("in loadDocumentsFromXML")
    for (xmlFilename ← xmlFilenames) { 
      println("xmlfile = " + (ROOT_CORPUS_DIR + xmlFilename));
      parseXML(corpus, XML.loadFile(ROOT_CORPUS_DIR + xmlFilename)) 
    }
  }

  // def extractWord(w: Node): Word = 
  // {
  //   new Word(w.text, (w \ "@lemma").text, (w \ "@type").text, w.text.toLowerCase.trim)
  // }

  //def extractPunctuation(c: Node): Word = new Word(c.text, c.text, (c \ "@type").text, c.text.trim)

  //def extractSentence(doc: Document, s: Node): Sentence = {
  def extractSentence(doc: Document, s: Node): Unit = {

    val number = (s \ "@n")(0).text
    //val wL = new ListBuffer[Word]();
    val wordNodes = new ListBuffer[Node]()
    val wordIds = new ListBuffer[Int]()

    s match {
      case <s>{ elements@_* }</s> ⇒
        for (element ← elements)
          element match {
            //case <w>{ _* }</w> | <c>{ _* }</c> => { println("element = \n" + element); wordNodes.append(element) }
            case <w>{ _* }</w> | <c>{ _* }</c> ⇒ wordNodes.append(element)
            case _ ⇒
          }
    }

    val sentenceId = sentences.size
    var wordIdx = 0
    var rawIdx = 0
    var cleanIdx = 0
    var posIdx = 0

    var positionIdxs = new ListBuffer[PositionIdx]
    var partofspeechSentence = ""
    for (wordNode ← wordNodes) {

      val rawStr = wordNode.toString
      val clean = wordNode.text
      val idxOf = wordNode.mkString.indexOf(clean)
      val sentenceIdx = SentenceIdx(sentenceId, wordIdx, rawIdx + idxOf, cleanIdx)
      //val sentenceIdx = SentenceIdx(sentenceId, wordIdx, idxOf, cleanIdx)
      val partofspeech = (wordNode \ "@type").text.trim
      partofspeechSentence += (partofspeech + " ")
      val lemma = (wordNode \ "@lemma").text.trim
      val trim = clean.trim.toLowerCase

      var wordId = -1;
      var addedToExistingWord = false

      //1. check and see if this word already exists in the global word map
      if (c2t.contains(trim)) {
        //ok this means that we have already stored a word that looks like this
        //let's check the set and see if there is one with the same POS
        //2. check to see if this word has the same part of speech as one already added
        for (t ← c2t(trim)) {
          if (t.clean == trim && t.partofspeech == partofspeech && t.raw == rawStr) {
            //2a. if yes, then add the current sentenceIdx to that existing word
              
            ////println("..." + trim + " already exists in the exact same format...")
            
            //w.sentences.append(sentenceIdx)
            addedToExistingWord = true
            wordId = t.id
          }
        }

        //2b. otherwise, if no, add this new word to the set (ie because it has a different POS or RAW,etc)
        if (addedToExistingWord == false) {
           //// println("..." + trim + " already exists but this is a new format for it...")
          //val LB = ListBuffer.empty[SentenceIdx]
          //LB.append(sentenceIdx)
          val newWord = Token(tokens.size, rawStr, trim, partofspeech, lemma)
          //val newWord = Word(words.size, trim, lemma, partofspeech, LB)
          tokens += newWord
          //words += newWord
          c2t(trim) += newWord
          wordId = newWord.id
        }
      } //3. create a new set of words with this word/pos and add it to the global word map
      else {
        //val LB = ListBuffer.empty[SentenceIdx]
        //LB.append(sentenceIdx)
          val newWord = Token(tokens.size, rawStr, trim, partofspeech, lemma)
          tokens += newWord
          //val newWord = Word(words.size, trim, lemma, partofspeech, LB)
          //words += newWord
          val wordSet = Set(newWord)
          c2t += (trim -> wordSet)
          wordId = newWord.id
      }

      /*
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
          //LB.append(sentenceIdx)
          val newWord = Word(words.size, trim, lemma, partofspeech, LB)
          words += newWord
          c2w(trim) += newWord
          wordId = newWord.wordId
        }
      } //3. create a new set of words with this word/pos and add it to the global word map
else {
        val LB = ListBuffer.empty[SentenceIdx]
        //LB.append(sentenceIdx)
        val newWord = Word(words.size, trim, lemma, partofspeech, LB)
        words += newWord
        val wordSet = Set(newWord)
        c2w += (trim -> wordSet)
        wordId = newWord.wordId
      }
      */

      positionIdxs += PositionIdx(sentenceId, wordIdx, 
              rawIdx, rawIdx + rawStr.trim.length, cleanIdx, cleanIdx + clean.trim.length, posIdx, posIdx + partofspeech.trim.length)
      // positionIdxs += PositionIdx(sentenceId, wordIdx, 
      //         rawIdx, rawIdx + rawStr.length, cleanIdx, cleanIdx + clean.length, posIdx, posIdx + partofspeech.length)
      // positionIdxs += PositionIdx(sentenceId, wordIdx, 
      //         rawIdx, rawIdx + rawStr.length - 1, cleanIdx, cleanIdx + clean.length - 1, posIdx, posIdx + partofspeech.length - 1)


      //4. increment the indexes
      wordIdx += 1;
      cleanIdx += clean.length;
      rawIdx += wordNode.toString.length;
      posIdx += partofspeech.length + 1
      wordIds += wordId
    }

    // println("s mkString = " + s.mkString)
    // println("s text     = " + s.text)
    // println("wordNodes  = " + wordNodes.mkString)

    //val sentence = Sentence(sentenceId, number, s.mkString, s.text, wordIds)
    val sentence = Sentence(sentenceId, number, wordNodes.mkString, s.text, partofspeechSentence.trim, wordIds, positionIdxs)
    
    sentences.append(sentence)

    doc.sentences += sentenceId

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

  /*
  //getting confused about the difference between Iterable and Collection and grabbing the Iterator directly?
  def buildMaps(ss: ListBuffer[Sentence],
                funcs: List[Word => String]): Iterator[Map[String, Int]] =
    //def buildMaps(ss : ListBuffer[Sentence], funcs: List[Word => String]): ListBuffer[Map[String, Int]] =
    {
      def makeMaps(): Map[Word => String, scala.collection.mutable.Map[String, Int]] =
        {
          val maps = Map.empty[Word => String, Map[String, Int]]

          for (f <- funcs)
            maps += (f -> Map.empty)

          return maps
        }

      def count(result: String, map: Map[String, Int]): Unit = {
        if (result != null) {
          val oldCount = if (map.contains(result)) { map(result) } else { 0 }
          map += (result -> (oldCount + 1))
        }
      }

      val maps = makeMaps

      for (s <- ss)
        for (w <- s.words)
          for (f <- maps.keys) count(f(w), maps(f))

      //return maps.values
      return maps.values.iterator
    }
  */

  def parseXML(corpus: Corpus, xml: NodeSeq): Unit =
    {
      val bncdoc = ((xml \\ "bncDoc")(0) \ "@id").text
      val title = (xml \\ "title")(0).text.trim
      println(bncdoc, title)
      val doc = Document(documents.size, bncdoc, title, ListBuffer.empty[Int])
      documents += doc

      corpus.documents += doc.documentId

      //not using these right now, but they are fun!
      // val getClean = (w: Word) => w.word //.toLowerCase
      // val getLemma = (w: Word) => w.lemma.toLowerCase
      // val getPOS = (w: Word) => w.pos.toUpperCase

      //for (s <- (xml \\ "s")) sentences.append(extractSentence(doc, s))
      for (s ← (xml \\ "s")) { extractSentence(doc, s) }

    }

  // 
  // def buildWordMap(si: SentenceIdx, w: Word): Unit = {
  //   w2s(w) = if (!w2s.contains(w)) { new ListBuffer[SentenceIdx] += si } else { w2s(w) += si }
  //   c2w(w.word) = if (!c2w.contains(w.word)) { new HashSet[Word] += w } else { c2w(w.word) += w }
  // }

}

