
package patterns

import corpus2.Driver._
import corpus2.{SentenceIdx, Word, Sentence, SentenceIdx2, Token}
import scala.collection.mutable._
import scala.util.matching.Regex

class SearchPattern(t:String) {
  val title = t

  var sentenceIds = new HashSet[Int]
  var sentenceIndexes = new HashSet[SentenceIdx2]
  var sentenceTokenPoss = new HashMap[Int, List[Int]]

   
  def findCleanTokens(s:Sentence, start:Int, end:Int) {
    println("sentence = \n" + s)
    println("start = " + start + ", end = " + end)
    
    val tis = for (p <- s.positionIdxs;
      if ( (start >= p.cleanStartIdx && start <= p.cleanEndIdx) ||
        (end >= p.cleanStartIdx && end <= p.cleanEndIdx) ||
        (start <= p.cleanStartIdx && end >= p.cleanEndIdx) )) yield {
        
        p.tokenIdx
            //tokens(s.tokens(p.tokenIdx))
      }

      sentenceTokenPoss += (s.id -> tis.toList)
    

  }
   
  def findCleanPatternMatches(regex : Regex) {
    for (s <- sentences; m <- regex findAllIn s.clean matchData) {
      sentenceIds += s.id
      sentenceIndexes += SentenceIdx2(s.id, m.start, m.end) 
      findCleanTokens(s, m.start, m.end)
      println(m.matched)
    }

    println(sentenceTokenPoss)

    for (s <- sentenceIds) {
    for (tp <- sentenceTokenPoss(s)) {
      //println(tp + " --- ")
      //println(sentences(s).tokens(tp))

      print(tokens(sentences(s).tokens(tp)).clean + " ")      
    }
    println
    
    for (tp <- sentenceTokenPoss(s)) {
      print(tokens(sentences(s).tokens(tp)).partofspeech + " ")      
    }
    println
    for (tp <- sentenceTokenPoss(s)) {
      print(tokens(sentences(s).tokens(tp)).raw + " ")      
    }
    println
    
    }
  }

  def findRawPatternMatches(regex : Regex) {
    for (s <- sentences; m <- regex findAllIn s.raw matchData) {
      sentenceIds += s.id
      sentenceIndexes += SentenceIdx2(s.id, m.start, m.end) 
      println(m.matched)
    }
  }
 
  /*
  def findRawPatternMatches(regex : Regex) {
    for (s <- sentences; m <- regex findAllIn s.raw matchData) {
      sentenceIds += s.sentenceId
      sentenceIndexes += SentenceIdx2(s.sentenceId, m.start, m.end) 
      println(m.matched)
    }
  }
  */

  def findMatchingSentences(idxs: List[Int]) : List[Int] = {
    idxs
  }
  
  def getPositionIndexesOfMatchingSentence(idxs: List[Int]) : List[Int] = {
    //this returns the position index of the matching sentences, rather than the sentence idx,
    //currently used for dispersion panel
    idxs 
  } 
}



class CleanSentenceRegexPattern(regex:Regex) extends SearchPattern("" + regex) {
  
  def this(regexStr: String) = this(regexStr.r)
 
  findCleanPatternMatches(regex)
  findTokens(regex)


  def findTokens(regex: Regex) {
    for (s <- sentences; m <- regex findAllIn s.clean matchData) {
      sentenceIds += s.id
      sentenceIndexes += SentenceIdx2(s.id, m.start, m.end) 
      println(m.matched)
    }

  }


  override def getPositionIndexesOfMatchingSentence(idxs: List[Int]) : List[Int] = {
    (for (i <- 0 until idxs.size; if (sentenceIds.contains(idxs(i)) ) ) yield i).toList
  }
   
  override def findMatchingSentences(idxs: List[Int]) : List[Int] = {
    //for now
    idxs
    
  }
}


class WordRegexPattern(regexStr:String) extends SearchPattern(regexStr) {

   var regex = regexStr.r

  var matchingWords = new HashSet[Word]
  for (w <- words; r <- regex findFirstIn w.word) {
    matchingWords += w
  }

  matchingWords foreach {w => print(w.word + ", ")}
  //println("matching words = " + matchingWords)

  for (w <- matchingWords; si <- w2s(w)) {
    //sentenceIndexes += si
    sentenceIds += si.sentenceId
  }

  override def getPositionIndexesOfMatchingSentence(idxs: List[Int]) : List[Int] = {
    (for (i <- 0 until idxs.size; if (sentenceIds.contains(idxs(i)) ) ) yield i).toList
  }
   
  override def findMatchingSentences(idxs: List[Int]) : List[Int] = {
    //for now
    idxs
    
  }
}

class WordPattern(word:String) extends SearchPattern(word) {
  
 // var test = """author|after"""

  findCleanPatternMatches(("""\b""" + word + """\b""").r)
  println("""all sentenceIdxs for " """ + word + """ " = """ + sentenceIndexes)   

  override def getPositionIndexesOfMatchingSentence(idxs: List[Int]) : List[Int] = {
    (for (i <- 0 until idxs.size; if (sentenceIds.contains(idxs(i)) ) ) yield i).toList
  }
   
  override def findMatchingSentences(idxs: List[Int]) : List[Int] = {
    //for now
    idxs
    
  }
}

class PosPattern(pos:String) extends SearchPattern(pos) {

  override def findMatchingSentences(idxs: List[Int]) : List[Int] = {
    //for now
    idxs
  }
}


class RawPattern(raw:String) extends SearchPattern(raw) {

  override def findMatchingSentences(idxs: List[Int]) : List[Int] = {
    //for now
    idxs
    
  }
}


