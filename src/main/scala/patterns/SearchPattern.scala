
package patterns

import corpus2.Driver._
import corpus2.{SentenceIdx, Word, Sentence, SentenceIdx2, Token, PositionIdx}
import scala.collection.mutable._
import scala.util.matching.Regex

 
object Mode extends Enumeration {
  type Mode = Value
  val CLEAN, RAW, POS = Value
}
import Mode._


case class ExactIdx(sid: Int, si: Int, ei: Int) {
  override def toString : String = "sid: " + sid + ", startIdx/endIdx: " + si + "/" + ei
}

abstract class SearchPattern(t:String, re:Regex, mode:Mode) {
  val title = t

  var sentence2matchIdxs = new HashMap[Int, ListBuffer[MatchIdx]]
  var sentence2exactIdxs = new HashMap[Int, ListBuffer[ExactIdx]]

  findPatternMatches

  case class MatchIdx(sid: Int, tokenPositions:List[Int]) {
    override def toString : String = "sid: " + sid + ", tokenPositions: " + tokenPositions
  }

  override def toString : String = {
    "title: " + title + ", regex: " + re + ", mode: " + mode
  }

  def getTokenForTokenPosition(sid: Int, tokenPosition: Int) : Token = {
    tokens(getTokenIdForTokenPosition(sid, tokenPosition))
  }

  def getTokenIdForTokenPosition(sid: Int, tokenPosition: Int) : Int = {
    sentences(sid).tokens(tokenPosition)
  }

  def getTokensForMatchIdx(mi: MatchIdx) : List[Token] = {
    //for (tp <- mi.tokenPositions) yield tokens(sentences(mi.sid).tokens(tp)) 
    for (tp <- mi.tokenPositions) yield getTokenForTokenPosition(mi.sid, tp) 
  }

  def getMode(s:Sentence) : String = {
    mode match {
      case CLEAN => s.clean
      case RAW => s.raw
      case POS => s.partofspeech
    }
  }

  def getPositionsForMode(pi:PositionIdx) : (Int, Int) = {
    mode match {
      case CLEAN => (pi.cleanStartIdx, pi.cleanEndIdx)
      case RAW => (pi.rawStartIdx, pi.rawEndIdx)
      case POS => (pi.posStartIdx, pi.posEndIdx)
    }
  }


  def findTokenPositions(eidx:ExactIdx) : List[Int] = {

    def checkIfTokensIntersectMatch(p:PositionIdx) : Boolean = {
      val (ps, pe) = getPositionsForMode(p);
      //println("[" + sentences(p.sentenceId).clean.substring(ps, pe) + "] ps/pe = " + ps + "/" + pe + " esi/eei = " + eidx.si + "/" + eidx.ei)

      def checkIfBeginningOverlaps : Boolean = (eidx.si >= ps && eidx.si < pe)
      def checkIfEndOverlaps : Boolean = (eidx.ei >= ps && eidx.ei <= pe)
      def checkIfContainedWithin : Boolean = (eidx.si <= ps && eidx.ei > pe)
     
      /*
      //debugging versions...
      def checkIfBeginningOverlaps : Boolean = {
        if (eidx.si >= ps && eidx.si < pe) {
          println("beginning overlaps...")
          true
        } else false
      }
      def checkIfEndOverlaps : Boolean = {
        if (eidx.ei >= ps && eidx.ei <= pe) {
          println("end overlaps...")
          true 
        } else false
      }
      def checkIfContainedWithin : Boolean = {
        if (eidx.si <= ps && eidx.ei > pe) {
          println("contained within...");
          true
        } else false
      }
      */
      ( checkIfContainedWithin || checkIfBeginningOverlaps || checkIfEndOverlaps ) 
    }

    (for (p <- sentences(eidx.sid).positionIdxs; if (checkIfTokensIntersectMatch(p))) yield p.tokenPosition).toList
  }


  def findPatternMatches {

    for (s <- sentences; m <- re findAllIn getMode(s) matchData) {
      //sentenceIds += s.id
      //sentenceIndexes += SentenceIdx2(s.id, m.start, m.end) 
      val ei = ExactIdx(s.id, m.start, m.end)
      sentence2exactIdxs(s.id) = if (!sentence2exactIdxs.contains(s.id)) { new ListBuffer[ExactIdx] += ei } else { sentence2exactIdxs(s.id) += ei }

      val mi = MatchIdx(s.id, findTokenPositions(ei))
      sentence2matchIdxs(s.id) = if (!sentence2matchIdxs.contains(s.id)) { new ListBuffer[MatchIdx] += mi } else { sentence2matchIdxs(s.id) += mi }

      //println(m.matched)
    }

    //println(sentence2matchIdxs)  
  }

  //ideas...
  //  store functions as variables, so easier to reuse
  //  functions should return a List, or something... and values should be put into the global hashmap elsewhere,
  // so we can reuse these functions

  // concordance needs to grab tokens ... hmm actually only with a mouse over/press... no need rects to already have this info

  //already have start+end idx (no need to do regex match), just need to get each token
  //s+e idx created in concordance using window



  def getCurrentMatchIdxs(idxs: List[Int]) : List[MatchIdx] = {
    val intersect = (sentence2matchIdxs.keySet.toList intersect idxs).sortWith{(a,b) => a < b}
    (for (i <- intersect) yield sentence2matchIdxs(i)).flatten
  }

  def getCurrentWindowedAndUnwindowedExactIdxs(idxs: List[Int], leftWindow:Int, rightWindow:Int) : List[(ExactIdx,ExactIdx)] = {
    for (eidx <- getCurrentExactIdxs(idxs)) yield {
      val uei = ExactIdx(eidx.sid, eidx.si, eidx.ei)
      val wei = ExactIdx(eidx.sid, math.max(eidx.si - leftWindow, 0), math.min(eidx.ei + rightWindow, getMode(sentences(eidx.sid)).length)) 
      (uei, wei)
    }
  }

  def getCurrentWindowedExactIdxs(idxs: List[Int], leftWindow:Int, rightWindow:Int) : List[ExactIdx] = {
    for (eidx <- getCurrentExactIdxs(idxs)) yield {
      ExactIdx(eidx.sid, math.max(eidx.si - leftWindow, 0), math.min(eidx.ei + rightWindow, getMode(sentences(eidx.sid)).length))
    }
  }

  def getCurrentExactIdxs(idxs: List[Int]) : List[ExactIdx] = {
    val intersect = (sentence2exactIdxs.keySet.toList intersect idxs).sortWith{(a,b) => a < b}
    (for (i <- intersect) yield sentence2exactIdxs(i)).flatten
  }

  //this returns the position index of the matching sentences, rather than the sentence idx,
  //currently used for dispersion panel
  def getPositionIndexesOfMatchingSentence(idxs: List[Int]) : List[Int] = {
    (for (i <- 0 until idxs.size; if (sentence2matchIdxs.keySet.contains(idxs(i)) ) ) yield i).toList
  }

  def printCurrentMatchedTokens(idxs: List[Int]) {
    val intersect = (sentence2matchIdxs.keySet.toList intersect idxs).sortWith{(a,b) => a < b}

    for (i <- intersect; mi <- sentence2matchIdxs(i); s = mi.sid; tps = mi.tokenPositions) {
      println("\n i = " + i + "mi = " + mi)
      for (tp <- tps) {
        val t = tokens(sentences(s).tokens(tp))
        print(" " + t)
      }
      println("")
    }
  }
}

class PartOfSpeechSentencePattern(regex:Regex) extends SearchPattern("" + regex, regex, POS) {
  def this(regexStr: String) = this(regexStr.r)
}
class CleanSentencePattern(regex:Regex) extends SearchPattern("" + regex, regex, Mode.CLEAN) {
  def this(regexStr: String) = this(regexStr.r)
}
class RawSentencePattern(regex:Regex) extends SearchPattern("" + regex, regex, RAW) {
  def this(regexStr: String) = this(regexStr.r)
}

class CleanTokenPattern(regex:Regex) extends SearchPattern("" + regex, ("""\b""" + regex + """\b""").r, CLEAN) {
def this(regexStr: String) = this(regexStr.r)
}

class PartOfSpeechTokenPattern(regex:Regex) extends SearchPattern("" + regex, ("""\b""" + regex + """\b""").r, POS) {
def this(regexStr: String) = this(regexStr.r)
}
