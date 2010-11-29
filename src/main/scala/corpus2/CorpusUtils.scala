package corpus2

import scala.util.parsing.combinator._
import scala.util.matching.Regex
import scala.xml._
import scala.collection.mutable._
import scala.util.control.Breaks._
import java.io.File

object CorpusUtils {
  def recursiveListFiles(f: File,
                         r: Regex): Array[File] = {
    val these = f.listFiles
    val good = these.filter(f ⇒ r.findFirstIn(f.getName).isDefined)
    good ++ these.filter(_.isDirectory).flatMap(recursiveListFiles(_, r))
  }

  def listXmlFiles(dir: String): Array[File] = {
    val dirFile = new File(dir)
    val these = dirFile.listFiles
    these.foreach(println)
    these
  }

  /* 
      Used for comparing equality in classes and customizing the comparisons in case classes.
    
      For example, in a class called "MyClass" with two values called "valA" and valB" that you
      want to use to test equality with, you would have these:

      override def equals(other: Any): Boolean = other match {
        case that: Sentence => (that canEqual this) && valA == that.valA && valB == that.valB
        case _ => false
      }
      def canEqual(other: Any): Boolean = other.isInstanceOf[MyClass]
      override def hashCode: Int = CorpusUtils.makeHash(valA, valB)

      If you use a case class and you want to use *all* of the main paramters to test for eqaulity,
      then this is done automatically and you don't need to have a equals, canEqual, or hashCode method.
  */
  def makeHash(objs: Any*): Int = {
    var hash = 0
    for (o ← objs) { hash += (41 * (o.hashCode + 41)) }

    hash
  }
}

