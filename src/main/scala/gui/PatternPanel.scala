package gui

import swing._ //{ MainPanel, Panel, Component, LayoutContainer, Orientation }
import swing.BorderPanel._
import swing.ScrollPane._
import swing.event._
import scala.Enumeration
import scala.collection.mutable._
import java.awt.{ Dimension, Color, Rectangle, Polygon, Shape, Toolkit }
import java.awt.image._
import javax.swing.RepaintManager
import java.awt.Font
import java.awt.FontMetrics
import java.awt.RenderingHints
import scala.swing.event.Key._
import javax.swing.ScrollPaneConstants
import javax.swing.SwingConstants
import javax.swing.ScrollPaneConstants._
import CorpusData._
import corpus2._
import corpus2.Driver._
import patterns._

//need to make sure mouse stuff doesn't happen if NO corpora attached to navigate panel...
class PatternPanel extends BorderPanel {
  val patterns = new ListBuffer[SearchPattern]
  var currentIdxs = List[Int]()

   def updatePanel(sIdxs: List[Int]) {
     println("in PatternPanel : updating currentIdxs...")

     currentIdxs = sIdxs
   }  
  
}

