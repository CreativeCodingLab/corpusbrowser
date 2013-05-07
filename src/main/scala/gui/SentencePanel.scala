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

//need to make sure mouse stuff doesn't happen if NO corpora attached to navigate panel...
class SentencePanel extends DataPanel {
  
   var updateAlways = true

  val MIN_WIDTH = 250 
  val MIN_HEIGHT = 100
  val WORD_FONT = new Font("Lucida Console", Font.PLAIN, 14); 
  val POS_FONT = new Font("Lucida Console", Font.PLAIN, 10); 
  
  var sentence:Sentence = null;
  var sentenceId:Int = -1;
  
  //prevHeight is used to check if revalidation is necessary
  var prevHeight = -1.0


  val display = new Display
  val control = new Control
  val jsp = new DisplayScrollPane(BarPolicy.Never, BarPolicy.AsNeeded) 
  add(jsp, Position.Center);
  add(control, Position.South);
  turnOffControlPanel

  listenTo(this)
  reactions += {
    case scala.swing.event.ComponentResized(src) => revalidateAll
    case _ =>
  }

   def clearPanel {
    this.sentence = null
    revalidateAll
  }

  class SentenceScrollPane(c : Component) extends ScrollPane(c) {
    border=null
    horizontalScrollBarPolicy = BarPolicy.Never
    verticalScrollBarPolicy = BarPolicy.AsNeeded
  }
 
  def updatePanel(sentenceId : Int) {
    this.sentenceId = sentenceId;
    if (sentenceId >= 0) { 
      this.sentence = Driver.sentences(sentenceId);
    } else {
      this.sentence = null
    }
    
    revalidateAll
  }

  class Display extends DisplayPanel {
    minimumSize = (MIN_WIDTH, MIN_HEIGHT)
    preferredSize = (MIN_WIDTH, MIN_HEIGHT)

    //assuming a word part-of-speech
    def getColorForPos(pos : String) : Color = {
      if (pos == null) {
        Color.GRAY;
      } else if (pos.startsWith("n")) {
        Color.RED;
      } else if (pos.startsWith("v")) {
        Color.BLUE;
      } else if (pos.startsWith("a")) {
        Color.GREEN;
      } else if (pos.startsWith("c")) {
        Color.ORANGE;
      } else if (pos.startsWith("p")) {
        Color.PINK;
      } else if (pos.startsWith("o")) {
        Color.YELLOW;
      } else if (pos.startsWith("c")) {
        Color.BLACK;
      } else {
        Color.GRAY;
      }
    }

    def getFontMetrics(g2: Graphics2D, font: Font) : FontMetrics = {
      g2.setFont(font)
      g2.getFontMetrics
    }

    override def paintComponent(g2: Graphics2D) {
      clear(g2)
      if (sentence != null) {
        displayWords(g2);
        //displaySentenceFingerprint(g2);

        //this just draws the sentence_id, later have more interesing stats
        //g2.setFont(bigFont);
        //g2.setColor(Color.BLACK);
        //int strw = metrics.stringWidth("" + sentence.sentence_id);
        //g.drawString("" + sentencePanel.getSentenceId(), getWidth() - 10 - strw,  41);
      }
    }

    //actually, store this as Rectangle2Ds or whatever so that they can be selcted, etc
    //Also, should be able to display tokens as well as words and pos, and
    //also lemmatized words (to do)
    def displayWords(g2 : Graphics2D) {
      val SPACE_BETWEEN_WORDS = 18
      val SPACE_BETWEEN_LINES = 35
      val LEFT_MARGIN = 21
      val RIGHT_MARGIN = 40
      val TOP_MARGIN = 30
      val POS_SUBSCRIPT = 12
      var xpos = LEFT_MARGIN;
      var ypos = TOP_MARGIN;
      var strw = 0;
      var startsWithPunc = false;
      var firstWordOfLine = true;
      var wordStr = ""
      var posStr = ""

      for (wordIdx <- sentence.tokens) {
        //wordStr = Driver.words(wordIdx).word
        //posStr = Driver.words(wordIdx).pos
        wordStr = Driver.tokens(wordIdx).clean
        posStr = Driver.tokens(wordIdx).partofspeech

        displayWord
        displayPosForWord

        incrementSpacing
      }

      revalidateIfNecessary(ypos + SPACE_BETWEEN_LINES)

      def incrementSpacing {
        xpos += strw + SPACE_BETWEEN_WORDS;
        firstWordOfLine = false;
        startsWithPunc = false;
      }

      def displayWord {
        strw = getFontMetrics(g2, WORD_FONT).stringWidth(wordStr);

        handlePunctuation
        handleEndOfLine

        g2.setColor(Color.DARK_GRAY);
        g2.drawString(wordStr, xpos, ypos);

      }

      def handlePunctuation {
        if (checkIfStartsWithPuncutation) {
          xpos -= SPACE_BETWEEN_WORDS;
          startsWithPunc = true;
        }
      }

      def handleEndOfLine {
        if (checkEndOfLine) {
          xpos = LEFT_MARGIN;
          ypos += SPACE_BETWEEN_LINES;
          firstWordOfLine = true;
        }
      }

      def checkEndOfLine : Boolean = {
        if (xpos + strw > size.width - RIGHT_MARGIN && !startsWithPunc) true else false
      }

      def checkIfStartsWithPuncutation : Boolean = {
        if (wordStr.matches("^[!\"#$%&'()*+,-./:;=?@\\[\\]^_`{|}~].*" ) ) true else false
      }

      def displayPosForWord {
        if (posStr != "PUN") { 
          var posMetrics = getFontMetrics(g2, POS_FONT)
          g2.setColor(getColorForPos(posStr.toLowerCase()))
          g2.drawString(posStr + " ",  (xpos + (strw / 2)) - (posMetrics.stringWidth(posStr) / 2), ypos + POS_SUBSCRIPT)
        }
      }

      def revalidateIfNecessary(ypos : Int) {
        if (ypos != prevHeight) {
          size.height = ypos
          preferredSize = (MIN_WIDTH, ypos);
          prevHeight = ypos
          revalidateAll
        } 
      }

    }
  }


   class Control extends ControlPanel("") {
      val labelT = new Label("add real panel soon...") {}
      add(labelT, "")
    }



}


