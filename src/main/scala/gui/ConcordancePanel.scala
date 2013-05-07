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

class ConcordancePanel extends PatternPanel {

  val LEFT_MARGIN = 5
  val RIGHT_MARGIN = 5
  val TOP_MARGIN = 0
  val BOTTOM_MARGIN = 0 
  val TITLE_MARGIN = 5
  val INC_MARGIN = 2
  val MIN_WIDTH = 250 
  val MIN_HEIGHT = 100

  val FONT_HEIGHT = 12;
  val FONT_WIDTH = 7;
  val LINE_HEIGHT = 15;

  val CONCORDANCE_FONT_PLAIN = new Font("Menlo", Font.PLAIN, 12)
  val CONCORDANCE_FONT_BOLD = new Font("Menlo", Font.BOLD, 12)
  val WINDOW_TEXT_COLOR = Color.DARK_GRAY
  val MATCH_TEXT_COLOR = Color.BLACK

  var longestRightMatch = -1

  val rects = new ListBuffer[Rect]
  val patternToLines : ConcurrentMap[SearchPattern, List[Line]] = new ConcurrentHashMap[SearchPattern, List[Line]] 
  var numSentenceIdxs = 0

  var leftWindow = 10
  var rightWindow = 10

  val display = new Display
  val control = new Control
  val jsp = new DisplayScrollPane(BarPolicy.AsNeeded, BarPolicy.AsNeeded)
  add(jsp, Position.Center);
  add(control, Position.South);
  turnOffControlPanel
 
  override def clearPanel {
    super.clearPanel
    patternToLines.clear
    rects.clear
    revalidateAll
  }

  override def updatePanel(sIdxs: List[Int]) {

    def resetVals {
      numSentenceIdxs = sIdxs.size
      longestRightMatch = 0
      patternToLines.clear
      rects.clear
    }

    super.updatePanel(sIdxs)

    resetVals

    for (p <- patterns) {
      patternToLines += p -> makeLines(p, sIdxs)
    }

    var i = 0;
    for ((p, lines) <- patternToLines; line <- lines) {
      rects ++= makeRects(line, i)
      i+=1
    }

    display.preferredSize = ((leftWindow + longestRightMatch) * FONT_WIDTH, TOP_MARGIN + ((i * LINE_HEIGHT) + BOTTOM_MARGIN))

    revalidateAll
  }

  case class Line(uei:ExactIdx, wei:ExactIdx, chunks:List[Chunk]) {
    override def toString = uei + "\n" + wei + "\n" + chunks
  }
  case class Chunk(tokenId:Int, str:String, si:Int, ei:Int) {
    override def toString = tokenId + ": [" + tokens(tokenId).clean + "] " + "[" + str + "] " + si + "/" + ei
  }
  case class Rect(chunk:Chunk, x:Int, y:Int, w:Int, h:Int, selectable:Boolean) {
    var isHovered = false
    var isSelected = false
    var hasBold = false
    var part1 = ""
    var part2 = chunk.str
    var part3 = ""
    var part1x = -1
    var part2x = -1
    var part3x = -1
    override def toString = x + "/" + y + "/" + w + "/" + h + " [" + chunk.str + "]"
  }

  def makeRects(line:Line, lineNum:Int) : ListBuffer[Rect] = {
    val rs = new ListBuffer[Rect]
    var y = TOP_MARGIN + (LINE_HEIGHT * lineNum);
    var x = 0;

    def markBoldLetters(r:Rect) {
      val c = r.chunk

      val startMatchIdx = line.uei.si
      val endMatchIdx = line.uei.ei
      val startChunkIdx = c.si
      val endChunkIdx = c.ei
      var startBoldIdx = 0
      var endBoldIdx = 0

      if (startMatchIdx >= startChunkIdx && startMatchIdx < endChunkIdx) {
        r.hasBold = true
        startBoldIdx = startMatchIdx - startChunkIdx
        if (endMatchIdx < endChunkIdx) {
          //  ----
          // ----
          endBoldIdx = c.str.length - (endChunkIdx - endMatchIdx)
        } else {
          //  ---
          // -----
          endBoldIdx = c.str.length 
        }
        markPositions(startBoldIdx, endBoldIdx)
      } else if (startChunkIdx >= startMatchIdx && startChunkIdx < endMatchIdx) {
        r.hasBold = true
        startBoldIdx = 0
        if (endChunkIdx > endMatchIdx) { 
          // ----
          //  ----
          endBoldIdx = c.str.length - (endChunkIdx - endMatchIdx)
        } else {
          // -----
          //  ---
          endBoldIdx = c.str.length 
        }
        markPositions(startBoldIdx, endBoldIdx)
      }

      def markPositions(startBoldIdx:Int, endBoldIdx:Int) {
        r.part1 = r.chunk.str.substring(0, startBoldIdx)
        r.part1x = r.x
        r.part2 = r.chunk.str.substring(startBoldIdx, endBoldIdx)
        r.part2x = r.x + (FONT_WIDTH * r.part1.length)
        r.part3 = r.chunk.str.substring(endBoldIdx, r.chunk.str.length)
        r.part3x = r.x + (FONT_WIDTH * (r.part1.length + r.part2.length))
      }
    }

    def makeDots(numDots:Int) : String = (for (i <- 0 until numDots) yield {"."}).mkString

    //calculate left dots
    val numLeftDots = leftWindow - (line.uei.si - line.wei.si)
    if (numLeftDots > 0) {
      rs += Rect(Chunk(-1, makeDots(numLeftDots), -1, -1), 0, y, FONT_WIDTH * numLeftDots, FONT_HEIGHT, false)
    }

    //calculate center rects
    for (c <- line.chunks) {
      val centerIdx = c.si - line.wei.si + numLeftDots

      val r = Rect(c, FONT_WIDTH * centerIdx, y, FONT_WIDTH * c.str.length, FONT_HEIGHT, true)
      rs += r

      markBoldLetters(r)
    }

    //calculate right dots
    val rightIdx = line.chunks.last.ei - line.wei.si + numLeftDots 
    val numRightDots = longestRightMatch - (line.wei.ei - line.uei.si)
    if (numRightDots > 0) {
      rs += Rect(Chunk(-1, makeDots(numRightDots), -1, -1), FONT_WIDTH * (rightIdx), y, FONT_WIDTH * numRightDots, FONT_HEIGHT, false)
    }

    rs
  }

  def makeLines(p:SearchPattern, sIdxs: List[Int]) : List[Line] = {

    def checkIfLongestRightMatch(uei:ExactIdx, wei:ExactIdx) = {
      val thisRight = if ( (uei.ei + rightWindow) > wei.ei) { (wei.ei - uei.si) } else { (uei.ei - uei.si) + rightWindow }
      if (thisRight > longestRightMatch) longestRightMatch = thisRight
    }

    for ((uei, wei) <- p.getCurrentWindowedAndUnwindowedExactIdxs(sIdxs, leftWindow, rightWindow)) yield {
      checkIfLongestRightMatch(uei, wei)
      Line(uei, wei, makeChunks(p, wei))
    }
  }

  def makeChunks(p:SearchPattern, ei:ExactIdx) : List[Chunk] = {
    val tps = p.findTokenPositions(ei)
    val sentence = sentences(ei.sid) 

    (for (i <- 0 until tps.size) yield {
        val position = (tps(i))
        val pi = sentence.positionIdxs(position)      
        val tokenId = sentence.tokens(pi.tokenPosition)
        val token = tokens(tokenId)
        val chunksi = pi.cleanStartIdx 
        val chunkei = if (i == tps.size - 1) ei.ei else pi.cleanEndIdx

        Chunk(tokenId, sentence.clean.substring(chunksi, chunkei), chunksi, chunkei)
      }).toList
  }

  def getFontMetrics(g2: Graphics2D, font: Font) : FontMetrics = {
    g2.setFont(font)
    g2.getFontMetrics
  }

  class Display extends DisplayPanel {
    minimumSize = (MIN_WIDTH, MIN_HEIGHT)
    preferredSize = (MIN_WIDTH, MIN_HEIGHT)
   
    def mouseInRect(r:Rect, x:Int, y:Int) : Boolean = {
      if (x > r.x && x < r.x + r.w && y > r.y && y < r.y + r.h) true else false
    }

    override def handlePressed(me: Point, modifiers: Int, clickCount: Int) {
      super.handlePressed(me, modifiers, clickCount)

      val x = me.getX.toInt
      val y = me.getY.toInt

      for (r <- rects; if (r.selectable)) {
        r.isSelected = if (mouseInRect(r, x, y)) true else false 
      }

      repaint
    }

    override def handleMoved(me: Point, mods: Int) {
      val x = me.getX.toInt
      val y = me.getY.toInt

      for (r <- rects; if (r.selectable)) {
        r.isHovered = if (mouseInRect(r, x, y)) true else false 
      }

      repaint
    }


    override def paintComponent(g2: Graphics2D) {
      clear(g2)

      val viewportRect = jsp.peer.getViewport.getViewRect() 

      val metrics = getFontMetrics(g2, CONCORDANCE_FONT_PLAIN)
      g2.setColor(Color.BLACK);

      for (r <- rects) {

        if (r.y + r.h > viewportRect.y && r.y < viewportRect.y + viewportRect.height) {

          if (r.isHovered) {
            g2.setColor(new Color(255,0,0, 200));
            g2.drawRect(r.x, r.y , r.w, r.h)
            g2.setColor(Color.BLACK);
          }

          if (r.isSelected) {
            g2.setColor(new Color(255,0,0, 128));
            g2.fillRect(r.x, r.y , r.w, r.h)
            g2.setColor(Color.BLACK);
          }

          if (r.hasBold) {
            g2.setColor(WINDOW_TEXT_COLOR);
            g2.setFont(CONCORDANCE_FONT_PLAIN)
            g2.drawString(r.part1, r.part1x, r.y + FONT_HEIGHT)

            g2.setColor(MATCH_TEXT_COLOR);
            g2.setFont(CONCORDANCE_FONT_BOLD)
            g2.drawString(r.part2, r.part2x, r.y + FONT_HEIGHT)

            g2.setColor(WINDOW_TEXT_COLOR);
            g2.setFont(CONCORDANCE_FONT_PLAIN)
            g2.drawString(r.part3, r.part3x, r.y + FONT_HEIGHT)
          } else {
            g2.setColor(WINDOW_TEXT_COLOR);
            g2.setFont(CONCORDANCE_FONT_PLAIN)
            g2.drawString(r.chunk.str, r.x, r.y + FONT_HEIGHT)
          }
        }
      }
    }
  }

  class Control extends ControlPanel("wrap 4") { 
    val hstr = ""//"h 20:20:20"
    val labelL = new Label("L") 
    add(labelL, hstr)

    val sliderL = new Slider {
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
      orientation = Orientation.Horizontal
      min              = 0
      value            = rightWindow
      max              = 50
      majorTickSpacing = 5
      paintTicks       = false
    }
    add(sliderR, hstr)

    listenTo(sliderL, sliderR, this.keys)
    reactions += {
      case ValueChanged(`sliderL`) => {
        leftWindow = sliderL.value
        updatePanel(currentIdxs) 
      }
      case ValueChanged(`sliderR`) => {
        rightWindow = sliderR.value
        updatePanel(currentIdxs)
      }
      case KeyPressed(src, key, modifiers, location) ⇒ handleKeyPress(key, modifiers)
      case _ =>
    }

  }
}
