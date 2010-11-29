package corpus2

import scala.util.parsing.combinator._
import scala.util.matching._
import scala.util.matching.Regex._
import scala.xml._
import scala.collection.mutable._
import scala.util.control.Breaks._
import java.io.File
//import corpus2.SentenceParser._
import corpus2.Driver._
import scala.math._

object SentenceQuerier {

  def findWordsWithClean(clean: String): Seq[Word] = c2w(clean) toBuffer

  def findRawExpressionIdxsWithExpression(re: Regex): Seq[ExpressionIdx] = for (s ← sentences; m ← re.findAllIn(s.raw).matchData.toList) yield ExpressionIdx(s.id, ExpressionType.RAW, m);

  def findCleanExpressionIdxsWithExpression(re: Regex): Seq[ExpressionIdx] = for (s ← sentences; m ← re.findAllIn(s.clean).matchData.toList) yield ExpressionIdx(s.id, ExpressionType.CLEAN, m);

  def findSentencesWithClean(clean: String): Seq[Sentence] = {
    (for (si ← findSentenceIdxsWithClean(clean)) yield sentences(si.sentenceId))
  }

  def countFrequency[T](seq: Seq[T]): Map[T, Int] = Map[T, Int]() ++ seq.groupBy(x ⇒ x).mapValues(_.length)

  def sortByValue[T, U <% Ordered[U]](unsorted: Map[T, U]): Map[T, U] = {
    val sortedMap = new LinkedHashMap[T, U]()
    val sortedKeys = unsorted.keysIterator.toList.sortWith((key1, key2) ⇒ unsorted(key1) > unsorted(key2))
    sortedKeys.foreach(key ⇒ sortedMap += (key -> (unsorted(key))))
    sortedMap
  }

  def findCollocations(clean: String): Map[Word, Int] = sortByValue(countFrequency(for (s ← findSentencesWithClean(clean); w ← s.words) yield words(w)))

  def findCollocationsWithinRange(clean: String, maxDist: Int): Map[Word, Int] =
    sortByValue(
      countFrequency(for (
        si ← findSentenceIdxsWithClean(clean);
        s = sentences(si.sentenceId);
        wi ← (max(0, si.wordIdx - maxDist)) until (min(s.words.size, si.wordIdx + maxDist + 1))
      ) yield words(s.words(wi))))

  def findSentenceIdxsWithClean(clean: String): Seq[SentenceIdx] = (for (w ← c2w.getOrElse(clean, return Seq.empty[SentenceIdx])) yield (w2s(w))).flatten.toBuffer

  def findSentenceIdxsWithLemma(lemma: String): Seq[SentenceIdx] = (for (w ← l2w.getOrElse(lemma, return Seq.empty[SentenceIdx])) yield (w2s(w))).flatten.toBuffer

  def makeCleanWindowForExpressionIdxs(exprs: Seq[ExpressionIdx], window: Int): Seq[String] = {
    for (e ← exprs; str = sentences(e.sentenceId).clean; m = e.exprMatch) yield makePrefix(str, m.start, window) + m.matched + makeSuffix(str, m.start, m.matched.length, window)
  }

  def makeRawWindowForExpressionIdxs(exprs: Seq[ExpressionIdx], window: Int): Seq[String] = {
    for (e ← exprs; str = sentences(e.sentenceId).raw; m = e.exprMatch) yield makePrefix(str, m.start, window) + m.matched + makeSuffix(str, m.start, m.matched.length, window)
  }

  def makeCleanWindowForSentenceIdxs(sents: Seq[SentenceIdx], window: Int): Seq[String] = {
    for (si ← sents; s = sentences(si.sentenceId); clean = s.clean; word = words(s.words(si.wordIdx)).word) yield makePrefix(clean, si.cleanIdx, window) + word + makeSuffix(clean, si.cleanIdx, word.length, window)
  }

  def makeRawWindowForSentenceIdxs(sents: Seq[SentenceIdx], window: Int): Seq[String] = {
    def makeCenter(str: String, si: SentenceIdx, len: Int): String = str.substring(si.rawIdx, si.rawIdx + len)

    for (si ← sents; s = sentences(si.sentenceId); raw = s.raw; word = words(s.words(si.wordIdx)).word) yield makePrefix(raw, si.rawIdx, window) + makeCenter(raw, si, word.length) + makeSuffix(raw, si.rawIdx, word.length, window)
  }

  def findCleanWindow(word: String, window: Int): Seq[String] = makeCleanWindowForSentenceIdxs(findSentenceIdxsWithClean(word), window)

  def findRawWindow(word: String, window: Int): Seq[String] = makeRawWindowForSentenceIdxs(findSentenceIdxsWithClean(word), window)

  private def makeDots(num: Int) = List.fill(num)(".") mkString

  private def makePrefix(str: String, idx: Int, window: Int): String = if (idx - window < 0) makeDots(window - idx) + str.substring(0, idx) else str.substring(idx - window, idx)

  private def makeSuffix(str: String, idx: Int, sentenceLen: Int, window: Int): String =
    if (idx + sentenceLen + window > str.length)
      str.substring(idx + sentenceLen, str.length) + makeDots(idx + sentenceLen + window - str.length)
    else
      str.substring(idx + sentenceLen, idx + sentenceLen + window)

  def findSentenceWithRegexOnClean(re: Regex): Seq[Sentence] = {
    for {
      s ← sentences;
      if (re.findFirstMatchIn(s.clean) match { case None ⇒ false; case _ ⇒ true })
    } yield s
  }

  def findSentenceWithRegexOnRaw(re: Regex): Seq[Sentence] = {
    for {
      s ← sentences;
      if (re.findFirstMatchIn(s.raw) match { case None ⇒ false; case _ ⇒ true })
    } yield s
  }

  def findWordsThatHaveMultiplePOS(): Seq[Word] = { (for (key ← c2w.keys; set = c2w(key); if (set.size > 1); w ← set) yield w).toBuffer }
}
