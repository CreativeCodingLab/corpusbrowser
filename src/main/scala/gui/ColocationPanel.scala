
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
import patterns.SearchPattern
import patterns.ExactIdx
import scala.collection.JavaConversions._
import java.util.concurrent.ConcurrentMap
import java.util.concurrent._

class ColocationPanel extends PatternPanel {

  val LEFT_MARGIN = 5
  val RIGHT_MARGIN = 15
  val BETWEEN_PATTERNS_WIDTH = 10
  val BOTTOM_MARGIN = 5
  val TOP_MARGIN = 5
  val HEADER_HEIGHT = 30
  val COLOCATION_HEIGHT = 40
  val BAR_HEIGHT = 20
  val TEXT_HEIGHT = 15


  //val mode = char or token...
  var leftWindow = 1
  var rightWindow = 1
  var minThreshold = 1 //minimum freq of colocation to be visualized... this should be set with a slider... 1 (ie, all) for now 

  val patternInfos = new ListBuffer[Info]

  val display = new Display
  val control = new Control
  val jsp = new DisplayScrollPane(BarPolicy.Never, BarPolicy.AsNeeded)
  add(jsp, Position.Center);
  add(control, Position.South);
  turnOffControlPanel



  override def handleResized(src:Component) {
    display.preferredSize = (size.width, TOP_MARGIN + HEADER_HEIGHT + (biggestSize * COLOCATION_HEIGHT) + BOTTOM_MARGIN)
    revalidateAll
  }

  def countFrequency[T](seq: Seq[T]): Map[T, Int] = Map[T, Int]() ++ seq.groupBy(x ⇒ x).mapValues(_.length)

  //highest on top...
  def sortByValue[T, U <% Ordered[U]](unsorted: Map[T, U]): Map[T, U] = {
    val sortedMap = new LinkedHashMap[T, U]()
    val sortedKeys = unsorted.keysIterator.toList.sortWith((key1, key2) ⇒ unsorted(key1) > unsorted(key2))
    sortedKeys.foreach(key ⇒ sortedMap += (key -> (unsorted(key))))
    sortedMap
  }


  def sortIntoListsOfTokens(token2count:Map[Int, Int]) : LinkedHashMap[Int, ListBuffer[Token]] = {
    val count2sortedTokens = new LinkedHashMap[Int, ListBuffer[Token]]

    for ((t,c) <- token2count) {
      //println(tokens(t).clean + " is colocated " + c + " times.")

      if (c >= minThreshold) {
        count2sortedTokens(c) = 
        if (!count2sortedTokens.contains(c)) { 
          new ListBuffer[Token] += tokens(t) 
        } else { 
          count2sortedTokens(c) += tokens(t) 
        }
      }
    }

    //okay lets sort, and also remove ones that don't satisfy our min threshold 
    for ((c,v) <- count2sortedTokens) {
      v.sortWith{(a,b) => a.clean < b.clean}
    }

    count2sortedTokens 
  }

  var biggestSize = 0

  override def clearPanel {
    super.clearPanel
    patternInfos.clear
    revalidateAll
  }

  override def updatePanel(sIdxs: List[Int]) {
    super.updatePanel(sIdxs)

    patternInfos.clear
    val tokenIds = new ListBuffer[Int]

    //test assuming TOKEN mode

    biggestSize = 0

    for (i <- 0 until patterns.size; p = patterns(i); mis = p.getCurrentMatchIdxs(sIdxs)) {
      for(mi <- mis) {
        val startTokenPos = mi.tokenPositions.head
        val endTokenPos = mi.tokenPositions.last

        val tStart = math.max(0, startTokenPos - leftWindow)
        val tEnd = math.min(sentences(mi.sid).tokens.size - 1, endTokenPos + rightWindow)

        for (i <- tStart to tEnd) {
          tokenIds += p.getTokenIdForTokenPosition(mi.sid, i)
        }
      }

      val token2count = sortByValue(countFrequency(tokenIds))
      val count2sortedTokens = sortIntoListsOfTokens(token2count)

      if (count2sortedTokens.size > 0) {
        makeRects(i, count2sortedTokens)
      }
    }

    display.preferredSize = (display.size.width, TOP_MARGIN + HEADER_HEIGHT + (biggestSize * COLOCATION_HEIGHT) + BOTTOM_MARGIN)
    revalidateAll
  }

  def makeRects(col:Int, count2sortedTokens :LinkedHashMap[Int, ListBuffer[Token]]){
    val rects = new ListBuffer[Rect]

    val biggest = count2sortedTokens.head._1
    val smallest = count2sortedTokens.last._1

    var py = TOP_MARGIN

    py += HEADER_HEIGHT
    var num = 0
    for ((c,lots) <- count2sortedTokens; t <- lots) {
      //println("count: " + c + " --> " + t.clean + " : minThreshold " + minThreshold)
      val perc = c.toDouble / biggest.toDouble
      rects += Rect(t, col, py + TEXT_HEIGHT, perc, BAR_HEIGHT)
      py += COLOCATION_HEIGHT 
      num+=1
    }

    if (num > biggestSize) {
      biggestSize = num
    }

    patternInfos += Info(col, patterns(col), rects)
  }

  case class Info(col:Int, pattern:SearchPattern, rects:ListBuffer[Rect]) {}

  case class Rect(token:Token, col:Int, y:Int, wperc:Double, h:Int) {}

  class Display extends DisplayPanel {

    override def paintComponent(g2: Graphics2D) {
      clear(g2)

       if (patternInfos.size <= 0) { return }

      //val metrics = getFontMetrics(g2, CONCORDANCE_FONT_PLAIN)

      val numPatterns = patterns.size
      val pw = ((size.getWidth - LEFT_MARGIN - RIGHT_MARGIN) / math.max(1, numPatterns)).toInt //avoid divide by zero if nothing loaded  

      val viewportRect = jsp.peer.getViewport.getViewRect() 

      for (pi <- patternInfos) { 
        val rx = BETWEEN_PATTERNS_WIDTH + (pw * pi.col)

        for (r <- pi.rects) {
          if (r.y > viewportRect.y && r.y < viewportRect.y + viewportRect.height) {
            val rw = ((pw - BETWEEN_PATTERNS_WIDTH) * r.wperc).toInt
            g2.setColor(new Color(255,0,0));
            g2.fillRect(rx, r.y, rw, r.h);
            g2.setColor(Color.BLACK);
            g2.drawString(r.token.clean, rx, r.y - 1)
          }

          //draw header
          g2.setColor(Color.WHITE);
          g2.fillRect(rx, viewportRect.y, pw, HEADER_HEIGHT);

          g2.setColor(Color.BLACK);
          g2.drawString(pi.pattern.title, rx, viewportRect.y + HEADER_HEIGHT - 2)
        }
      }

      //draw border for heaader
      g2.setColor(Color.BLACK);
      g2.drawLine(
        viewportRect.x + 4, viewportRect.y + HEADER_HEIGHT, 
        viewportRect.width - 4, viewportRect.y + HEADER_HEIGHT
      )

  }

}

class Control extends ControlPanel("wrap 6") { 
  val labelT = new Label("Threshold") 
  add(labelT, hstr)

  val sliderT = new Slider {
    peer.putClientProperty( "JComponent.sizeVariant", "small" );
    orientation = Orientation.Horizontal
    min              = 1
    value            = minThreshold
    max              = 50
    majorTickSpacing = 5
    paintTicks       = false
  }
  add(sliderT, hstr)

  val hstr = ""//"h 20:20:20"
  val labelL = new Label("L") 
  add(labelL, hstr)

  val sliderL = new Slider {
    peer.putClientProperty( "JComponent.sizeVariant", "small" );
    orientation = Orientation.Horizontal
    min              = 0
    value            = leftWindow
    max              = 50
    majorTickSpacing = 5
    paintTicks       = false
  }
  add(sliderL, hstr)

  val labelR = new Label("R") 
  add(labelR, hstr)

  val sliderR = new Slider {
    peer.putClientProperty( "JComponent.sizeVariant", "small" );
    orientation = Orientation.Horizontal
    min              = 0
    value            = rightWindow
    max              = 50
    majorTickSpacing = 5
    paintTicks       = false
  }
  add(sliderR, hstr)

  listenTo(sliderL, sliderR, sliderT, this.keys)
  reactions += {
    case ValueChanged(`sliderL`) => {
      leftWindow = sliderL.value
      updatePanel(currentIdxs) 
    }
    case ValueChanged(`sliderR`) => {
      rightWindow = sliderR.value
      updatePanel(currentIdxs)
    }
    case ValueChanged(`sliderT`) => {
      minThreshold = sliderT.value
      updatePanel(currentIdxs)
    }
    case _ =>
  }


}

}
