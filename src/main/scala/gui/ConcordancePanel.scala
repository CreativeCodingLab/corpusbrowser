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

   implicit def tuple2Dimension(tuple: Tuple2[Int, Int]) = new Dimension(tuple._1, tuple._2)
  
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

  val CONCORDANCE_FONT = new Font("Menlo", Font.PLAIN, 12)
  val display = new Display
  val control = new Control

  //val linesToRects : ConcurrentMap[Line, List[Rect]] = new ConcurrentHashMap[Line, List[Rect]] 
  val rects = new ListBuffer[Rect]
  val patternToLines : ConcurrentMap[SearchPattern, List[Line]] = new ConcurrentHashMap[SearchPattern, List[Line]] 
  val patternToExactIdxs : ConcurrentMap[SearchPattern, List[ExactIdx]] = new ConcurrentHashMap[SearchPattern, List[ExactIdx]] 
  var numSentenceIdxs = 0

  var leftWindow = 20
  var rightWindow = 10

  val jsp = new ConcordanceScrollPane(display) 
  add(jsp, Position.Center);
  add(control, Position.South);
  
  listenTo(this)
  reactions += {
    case scala.swing.event.ComponentResized(src) => revalidateAll
    case _ =>
  }

 class ConcordanceScrollPane(c : Component) extends ScrollPane(c) {
    border=null
    horizontalScrollBarPolicy = BarPolicy.Never
    verticalScrollBarPolicy = BarPolicy.AsNeeded
  }

  def revalidateAll {
    jsp.revalidate;
    jsp.repaint;
    display.revalidate;
    display.repaint;
  }
  
  override def updatePanel(sIdxs: List[Int]) {
    super.updatePanel(sIdxs)
    numSentenceIdxs = sIdxs.size
    
    patternToLines.clear
    rects.clear
    for (p <- patterns) patternToLines += p -> makeLines(p, sIdxs)
     
    var i = 0;
    for ((p, lines) <- patternToLines; line <- lines) {
      //println (p + "-->" + line) 
      rects ++= makeRects(line, i) //will be added to linesToRects which is what will be used in drawing/selecting
      i+=1
    }

    display.preferredSize = (size.width, TOP_MARGIN + ((i-0) * LINE_HEIGHT) + BOTTOM_MARGIN)
    

    //for (p <- patterns) { p.printCurrentMatchedTokens(sIdxs) }
     // for ((p, mis) <- patternToExactIdxs; mi <- mis) {
     //    println("in CP, mi = " + mi) 
     //    println(sentences(mi.sid).clean.substring(mi.startIdx, mi.endIdx))
     //   }

     revalidateAll
    //repaint
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
    var part1 = ""
    var part2 = chunk.str
    var part3 = ""
    var part1x = -1
    var part2x = -1
    var part3x = -1
    var hasBold = false
    override def toString = x + "/" + y + "/" + w + "/" + h + " [" + chunk.str + "]"
  }

  def makeRects(line:Line, lineNum:Int) : ListBuffer[Rect] = {
    val rs = new ListBuffer[Rect]
    var y = TOP_MARGIN + (LINE_HEIGHT * lineNum);
    var x = 0;

     def setBold(r:Rect) {
      val c = r.chunk

      //if (
      if (c.si >= line.uei.si && c.si < line.uei.ei) {
        r.hasBold = true
        val sb = c.si - line.uei.si

        println("in setBold for chunk [" + c.str + "] ... sb = " + sb)
        //val eb = sb + 1//c.ei - (line.uei.ei - c.ei)
        val eb = c.str.length - (c.ei - line.uei.ei)
        println("in setBold for chunk [" + c.str + "] ... sb/eb = " + sb + "/" + eb)
        //val eb = sb + 1//c.ei - (line.uei.ei - c.ei)

        r.part1 = r.chunk.str.substring(0, sb)
        r.part1x = r.x
        r.part2 = r.chunk.str.substring(sb, eb)
        r.part2x = r.x + (FONT_WIDTH * r.part1.length)
        r.part3 = r.chunk.str.substring(eb, r.chunk.str.length)
        r.part3x = r.x + (FONT_WIDTH * (r.part1.length + r.part2.length))
      }
    }

    //calculate left dots if needed and update xoff if needed
    var dotLoff = 0
    val usingL = line.uei.si - line.wei.si
    if (usingL != leftWindow) {
      val numdotsL = leftWindow - usingL 

      var dotsL = ""
      for (i <- 0 until numdotsL) { dotsL += "." }
      val r = Rect(Chunk(-1, dotsL, -1, -1), 0, y, dotsL.length * FONT_WIDTH, FONT_HEIGHT, false)
      rs += r
      dotLoff = numdotsL
    }

    var endletter = -1
    for (c <- line.chunks) {
      var xoff = c.si - line.wei.si + dotLoff

      //val r = Rect(c, (FONT_WIDTH * c.si) - (FONT_WIDTH * line.wei.si), y, c.str.length * FONT_WIDTH, FONT_HEIGHT)
      val r = Rect(c, FONT_WIDTH * xoff, y, c.str.length * FONT_WIDTH, FONT_HEIGHT, true)
      rs += r

      setBold(r)
      endletter = xoff + c.str.length //better way to do this...
    }

    //calculate right dots
    val usingR = line.wei.ei - line.uei.ei
    if (usingR != rightWindow) {
      val numdotsR = rightWindow - usingR 
      var dotsR = ""
      for (i <- 0 until numdotsR) { dotsR += "." }
      val r = Rect(Chunk(-1, dotsR, -1, -1), FONT_WIDTH * (endletter), y, dotsR.length * FONT_WIDTH, FONT_HEIGHT, false)
      rs += r


    }

   
    rs
  }

  def makeLines(p:SearchPattern, sIdxs: List[Int]) : List[Line] = {
    for ((uei, wei) <- p.getCurrentWindowedAndUnwindowedExactIdxs(sIdxs, leftWindow, rightWindow)) yield {
      //println("in makeLines: " + wei.si + "-->" + wei.ei + " : " + sentences(wei.sid).clean.substring(wei.si, wei.ei))

      Line(uei, wei, makeChunks(p, wei))
    }
  }

  def makeChunks(p:SearchPattern, ei:ExactIdx) : List[Chunk] = {
    val tps = p.findTokenPositions(ei)
    println("in makeChunk: chunks = " + tps)
    val sentence = sentences(ei.sid) 
    println("in makeChunk: window = [" + sentence.clean.substring(ei.si, ei.ei) + "]") 

    (for (i <- 0 until tps.size) yield {

        val position = (tps(i))
        val pi = sentence.positionIdxs(position)      
        val tokenId = sentence.tokens(pi.tokenPosition)
        val token = tokens(tokenId)
        val chunksi = pi.cleanStartIdx //if (i == 0) ei.si else pi.cleanStartIdx
        val chunkei = if (i == tps.size - 1) ei.ei else pi.cleanEndIdx

        println("in makeChunk : tokenpos " + i + " = " + chunksi + "-->" + chunkei + "   :   " + token.clean)
        Chunk(tokenId, sentence.clean.substring(chunksi, chunkei), chunksi, chunkei)
      }).toList
  }

  def getFontMetrics(g2: Graphics2D, font: Font) : FontMetrics = {
    g2.setFont(font)
    g2.getFontMetrics
  }

  class Display extends Component {
    minimumSize = (MIN_WIDTH, MIN_HEIGHT)
    preferredSize = (MIN_WIDTH, MIN_HEIGHT)

    listenTo(this.mouse.clicks, this.mouse.moves, this.keys)
    reactions += {
      case MouseMoved(src, point, mods) ⇒ {handleMoved(point, mods)}
      case MousePressed(src, point, i1, i2, b) ⇒  handlePressed(point, i1, i2) 
      case KeyPressed(src, key, modifiers, location) ⇒ Main.handleKeyPress(key, modifiers)
      case _ =>
    }

    def mouseInRect(r:Rect, x:Int, y:Int) : Boolean = {
      if (x > r.x && x < r.x + r.w && y > r.y && y < r.y + r.h) true else false
    }

    def handlePressed(me: Point, modifiers: Int, clickCount: Int) {
      requestFocus

      val x = me.getX.toInt
      val y = me.getY.toInt

      for (r <- rects; if (r.selectable)) {
        r.isSelected = if (mouseInRect(r, x, y)) true else false 
      }

      repaint
    }

    def handleMoved(me: Point, mods: Int) {
      val x = me.getX.toInt
      val y = me.getY.toInt

      for (r <- rects; if (r.selectable)) {
        r.isHovered = if (mouseInRect(r, x, y)) true else false 
      }

      repaint
    }


    def clear(g2: Graphics2D) {
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2.setColor(Color.WHITE);
      g2.fillRect(0,0, size.width, size.height);
    }

    override def paintComponent(g2: Graphics2D) {
      clear(g2)

      val metrics = getFontMetrics(g2, CONCORDANCE_FONT)
      g2.setColor(Color.BLACK);

      for (r <- rects) {
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
          g2.setColor(Color.BLACK);
          g2.drawString(r.part1, r.part1x, r.y + FONT_HEIGHT)
          g2.setColor(Color.GREEN);
          g2.drawString(r.part2, r.part2x, r.y + FONT_HEIGHT)
          g2.setColor(Color.BLACK);
          g2.drawString(r.part3, r.part3x, r.y + FONT_HEIGHT)
        } else {
          g2.setColor(Color.BLACK);
          g2.drawString(r.chunk.str, r.x, r.y + FONT_HEIGHT)
        }
      }
    }
  }

  class Control extends Component {

  }





}
