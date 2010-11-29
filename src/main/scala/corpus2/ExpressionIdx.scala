package corpus2

import com.novus.casbah.mongodb.MongoConnection
import com.novus.casbah.mongodb.Imports._

import scala.util.parsing.combinator._
import scala.util.matching.Regex
import scala.util.matching.Regex._
import scala.util.matching._
import scala.xml._
import scala.collection.mutable._
import scala.util.control.Breaks._
import java.io.File
import com.novus.casbah.mongodb.Imports._

//object ExpressionIdx {}

object ExpressionType extends Enumeration {
  type ExpressionType = Value
  val CLEAN, RAW = Value
}
import ExpressionType._

case class ExpressionIdx(sentenceId: Int, exprType: ExpressionType, exprMatch: Match) {
  override def toString(): String = "[" + sentenceId + ":" + exprType + ":" + exprMatch.matched + "@" + exprMatch.start + "]"
}

