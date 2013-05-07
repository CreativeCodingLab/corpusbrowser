package gui

import scala.collection.mutable._
import swing._ //{ MainPanel, Panel, Component, LayoutContainer, Orientation }
import swing.event._
import javax.swing.border._
import javax.swing.border.Border._
import scala.Enumeration
import java.awt.{ Dimension, Color }
import javax.swing.JSeparator
import javax.swing.{JInternalFrame, JDesktopPane}
import javax.swing.SwingConstants
import scala.swing.event.Key._
import CorpusData._
import org.interactivemesh.scala.swing._
import org.interactivemesh.scala.swing.event._
import org.interactivemesh.scala.swing.LayeredPane._
import org.interactivemesh.scala.swing.InternalDesktopPane._
import java.io._
import corpus2._
import corpus2.Driver._
import patterns._
import gui._


class ViewTab(num:Int) {
  
  var component:Component = _

  val sentencePanels = new ListBuffer[SentencePanel] //all of these need to be updated when the navigate panel current sentence changes
  val patternPanels = new ListBuffer[PatternPanel] //all of these need to be updated when the navigate panel view changes
  

  def clearPanels {
    for (sp <- sentencePanels) {
      sp.clearPanel
    }

    for (pp <- patternPanels) {
      pp.clearPanel
    }
     
  }

  def updateSentencePanels(sIdx: Int, updateOnMove:Boolean, updateOnRelease:Boolean) {
    for (sp <- sentencePanels) {
      if ( (updateOnMove && sp.updateAlways) || (updateOnRelease && !sp.updateAlways) ) {
        sp.updatePanel(sIdx)
      }
    }
  }

  def updatePatternPanels(sIdxs: List[Int], updateOnMove:Boolean, updateOnRelease:Boolean)  {
    for (pp <- patternPanels) {
      if ( (updateOnMove && pp.updateAlways) || (updateOnRelease && !pp.updateAlways) ) {
        pp.updatePanel(sIdxs)
      }
    }
  }

}
