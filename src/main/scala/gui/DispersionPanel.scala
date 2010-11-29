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
import scala.collection.JavaConversions._
import java.util.concurrent.ConcurrentMap
import java.util.concurrent._

//need to make sure mouse stuff doesn't happen if NO corpora attached to navigate panel...
class DispersionPanel extends PatternPanel {

  val LEFT_MARGIN = 5
  val RIGHT_MARGIN = 5
  val TOP_MARGIN = 5
  val BOTTOM_MARGIN = 25
  val TITLE_MARGIN = 5
  val INC_MARGIN = 2

  val display = new Display
  val control = new Control

  add(display, Position.Center);
  add(control, Position.South);

  val patternToPositionIdxs : ConcurrentMap[SearchPattern, List[Int]] = new ConcurrentHashMap[SearchPattern, List[Int]] 
  
  var numSentenceIdxs = 0

  override def updatePanel(sIdxs: List[Int]) {
    super.updatePanel(sIdxs)
    numSentenceIdxs = sIdxs.size
    
    patternToPositionIdxs.clear
    for (p <- patterns) { patternToPositionIdxs += p -> p.getPositionIndexesOfMatchingSentence(sIdxs) } 

    repaint
  }

  def getFontMetrics(g2: Graphics2D, font: Font) : FontMetrics = {
    g2.setFont(font)
    g2.getFontMetrics
  }

  class Display extends Component {

    def clear(g2: Graphics2D) {
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2.setColor(Color.WHITE);
      g2.fillRect(0,0,size.width, size.height);
    }

    override def paintComponent(g2: Graphics2D) {

      clear(g2)

      val sectionWidth = dividerWidth(patterns.size)
      val plotPerc = calculatePlotPercent
      val colBottom = makeBottom 
      val colTitleY = makeTitleY 
      val colWidth = makeWidth(sectionWidth)
      val rectHeight = if (numSentenceIdxs > 0) {calculatePlotHeight / numSentenceIdxs} else 0

      for (i <- 0 until patterns.size; p = patterns(i)) {
        val colLeft = makeLeft(i, sectionWidth); 

        drawBorder

        if (patternToPositionIdxs.size > 0) {
          drawRects
        }

        def drawRects {
          g2.setColor(new Color(0,0,0,128));

          if (patternToPositionIdxs.contains(p)) {
            val posList = patternToPositionIdxs(p)
            for (posIdx <- 0 until posList.size) {
              g2.fillRect(colLeft, (colBottom - (posList(posIdx) * plotPerc) - (rectHeight*.75) ).toInt, colWidth, math.max(1,rectHeight*.5).toInt)
            }
          }
        }

        def drawBorder {
          g2.setColor(Color.BLACK);
          g2.drawRect(colLeft, colBottom + 3, colWidth, 2)
          g2.drawString(p.title, colLeft, colTitleY)
        }
      }
    }

    def calculatePlotHeight : Int = {
      (size.height - BOTTOM_MARGIN) - TOP_MARGIN
    }

    def calculatePlotPercent : Double = {
      calculatePlotHeight / numSentenceIdxs.toDouble
    }

    def makeTitleY : Int = {
      size.height - TITLE_MARGIN
    }

    def dividerWidth(n : Int) : Int = {
      (size.width - (LEFT_MARGIN + RIGHT_MARGIN)) / n
    }

    def makeWidth(w : Int) : Int = {  
      w - (INC_MARGIN * 2)
    }

    def makeBottom : Int = {
      size.height - BOTTOM_MARGIN
    }

    def makeLeft(idx : Int, sectionWidth: Int) : Int = {  
      LEFT_MARGIN + (sectionWidth * idx) + INC_MARGIN
    }



  }

  class Control extends Component {

  }

}
