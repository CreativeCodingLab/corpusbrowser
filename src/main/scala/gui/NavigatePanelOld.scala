package gui


import swing._ //{ MainPanel, Panel, Component, LayoutContainer, Orientation }
import swing.event._
import javax.swing.border._
import javax.swing.border.Border._
import scala.Enumeration
import scala.collection.mutable._
import java.awt.{ Dimension, Color, Rectangle, Polygon, Shape, Toolkit }
import java.awt.image._
import javax.swing.JSeparator
import javax.swing.RepaintManager
import java.awt.Font
import java.awt.FontMetrics
import java.awt.RenderingHints
import scala.swing.event.Key._
import javax.swing.SwingConstants
import CorpusData._

class NavigatePanelOld extends Panel{
  var leftView = 0
  var rightView = 0
  var leftWindow = 0
  var rightWindow = 0
  var maxView = 0
  var viewRange = 1
  var windowRange = 1
  var currentSentence = 25
  var zoomPercent = .5
  var viewPercent = .5
  var windowShape = new Rectangle(0, 0, 0, 0)
  var windowShapeSelected = false
  var leftWindowSizerShape = new Rectangle(0, 0, 0, 0)
  var leftWindowSizerShapeSelected = false
  var rightWindowSizerShape = new Rectangle(0, 0, 0, 0)
  var rightWindowSizerShapeSelected = false
  var zoomShape = new Rectangle(0, 0, 0, 0)
  var zoomShapeSelected = false
  var viewShape = new Rectangle(0, 0, 0, 0)
  var viewShapeSelected = false
  //Rectangle sentenceShape = new Rectangle(0,0,0,0)
  var sentenceShape = new Polygon; //(new Rectangle(0, 0, 0, 0))
  var sentenceShapeSelected = false
  var zoomDown = false
  var zoomUp = false

  var prevx = 0
  var prevy = 0

  var g_isJogging = false
  var isFullyExpanded = false
  
  //margins not used yet...
  var topMargin = 30
  var bottomMargin = 30
  var leftMargin = 0
  var rightMargin = 0

  var prevZoomPercent = 0.0
  var prevViewPercent = 0.0
  var prevLeftWindow = 0
  var prevRightWindow = 0
  var prevLeftView = 0
  var prevRightView = 0
  var prevMaxView = 0
  var prevCurrentSentence = 0
  var prevSentenceNums = new ListBuffer[Int]

  var offx = 0
  var offy = 0
  var numThreads = 0

  var CORPUS_INDICATOR_FONT = new Font("Menlo", Font.PLAIN, 12)
  //opaque = true
  //peer.setOpaque(true)
  //peer.setDoubleBuffered(true)
  //peer.setIgnoreRepaint(true);
  focusable=true
  border=null
  
  // var inited = false
  // var b = new BufferedImage(
  //   1, 1, BufferedImage.TYPE_INT_ARGB);

  var rm = RepaintManager.currentManager(this.peer); //.setDoubleBufferingEnabled(false);
  rm.setDoubleBufferingEnabled(true);


  initializeRangeSettings


  def clearBackground(g2: Graphics2D) {
    g2.setColor(Color.WHITE)
    g2.fillRect(0, 0, size.getWidth.toInt, size.getHeight.toInt)
  }

  def getFontMetrics(g2: Graphics2D) : FontMetrics = {
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2.setFont(CORPUS_INDICATOR_FONT)
    g2.getFontMetrics
  }

  def drawNothingLoadedMessage(g2: Graphics2D, metrics: FontMetrics) {
    g2.setColor(Color.GRAY)
    g2.fillRect(0, 0, size.getWidth.toInt, size.getHeight.toInt)
    g2.setColor(Color.BLACK)
    val emptyMsg = "no sentences available"
    val emptyMsgW = metrics.stringWidth(emptyMsg)
    val emptyMsgH = metrics.getHeight
    //g2.drawString(emptyMsg, (windowShape.getX + (windowShape.getWidth/2) - (emptyMsgW / 2)).toInt, (windowShape.getY + (windowShape.getHeight/2) + (emptyMsgH/2)).toInt)
  }

  def drawCorpusIndicators(g2: Graphics2D, metrics: FontMetrics) {
    //println("currentCorpusNodes.size = " + currentCorpusNodes.size)

    g2.setFont(CORPUS_INDICATOR_FONT)

    for (i <- 0 until currentCorpusNodes.size) {
      val cd = currentCorpusNodes(i)
      //println("cd = " + cd)
      val spix = getLeftSentencePixel(cd.startIdx)
      val epix = getRightSentencePixel(cd.endIdx)
      //println("spix/epix = " + spix + "/" + epix)

      if (!(epix < 0 || spix > size.getWidth)) {

        g2.setColor(cd.color)
        //g2.setColor(Color.RED); //really will use an appropriate color...
        g2.fillRect(spix, 0, epix - spix, size.getHeight.toInt)
        
        g2.setColor(Color.GRAY)
        //g2.drawRect(spix, 0, epix - spix, size.getHeight.toInt)
        if (spix > 0) {
          g2.drawLine(spix, 0, spix, size.getHeight.toInt)
        }
        if (epix < size.getWidth) {
          g2.drawLine(epix, 0, epix, size.getHeight.toInt)
        }
        g2.setColor(Color.BLACK)

        if (metrics.stringWidth(cd.name) + 10 < (epix - spix)) {
          g2.drawString(cd.name, spix + ((epix - spix) / 2) - (metrics.stringWidth(cd.name).toInt / 2), size.getHeight.toInt - 5)
        }
      }
    }
  }

  def drawSentenceIndexes(g2: Graphics2D, metrics: FontMetrics ) {
    val inc = getNotchIncrement
    for (i <- 0 until leftView + viewRange) {
      val notchPix = getExactSentencePixel(i)

      if (i % inc == 0) {
        g2.setColor(Color.BLACK)
        val notchWidth = metrics.stringWidth("" + i)
        g2.setFont(CORPUS_INDICATOR_FONT)
        g2.drawString("" + i, notchPix - (notchWidth / 2), (size.getHeight * .66).toInt)
        g2.drawRect(notchPix, (size.getHeight * .66).toInt - metrics.getHeight - 7, 0, 7)
      } else {
        //g2.setColor(Color.GRAY)
        //g2.drawRect(notchPix, (size.getHeight/2).toInt, 0, 4)
      }
    }
  }


  def drawSelectionWindow(g2: Graphics2D, metrics: FontMetrics ) {
    g2.setColor(new Color(255, 255, 255, 194))
    g2.fill(windowShape)

    //g2.setColor(Color.BLACK)
    //g2.fillRect( (windowShape.getX).toInt, (windowShape.getY + (windowShape.getHeight / 2) - 5).toInt, 10, 10)
    //g2.fillRect( (windowShape.getX + windowShape.getWidth - 10).toInt, (windowShape.getY + (windowShape.getHeight / 2) - 5).toInt, 10, 10)

    val numS = "" + (rightWindow - leftWindow + 1)
    val numSW = metrics.stringWidth(numS)
    val numSH = metrics.getHeight

    g2.setColor(Color.BLACK)
    g2.drawString(numS, (windowShape.getX + (windowShape.getWidth / 2) - (numSW / 2)).toInt, (windowShape.getY + (windowShape.getHeight / 2) + (numSH / 2)).toInt)

    g2.setColor(Color.BLACK)
    g2.draw(windowShape)
  }


  def drawNavigationHandles(g2: Graphics2D) {
    //draw sizer handles
    g2.setColor(Color.BLACK)
    g2.fill(leftWindowSizerShape)
    g2.fill(rightWindowSizerShape)

    //draw zoom window
    g2.setColor(Color.BLUE)
    //g2.fill(zoomShape)

    //draw window slider
    g2.setColor(Color.GREEN)
    //g2.fill(viewShape)

    //draw sentence selector
    g2.setColor(Color.BLACK)
    g2.fill(sentenceShape)

  }
  def paintNow() {
     rm.markCompletelyDirty(this.peer)
     repaint
  }

  override def paintComponent(g2: Graphics2D) {
  // super.paintComponent(g2)
  // println(rm)
  // println(rm.isDoubleBufferingEnabled())
   
   resetShapePositions


  val metrics = getFontMetrics(g2)
   
    /*
    if (inited == false) {
      b = new BufferedImage(
        size.getWidth().toInt, size.getHeight().toInt, BufferedImage.TYPE_INT_ARGB);

      inited = true
    }
    var offscreen = b.getGraphics().asInstanceOf[Graphics2D];
    //offscreen.setColor(Color.WHITE);
    clearBackground(offscreen)

    drawSelectionWindow(offscreen, metrics)
    offscreen.dispose();
    g2.drawImage(b, 0, 0, null);
    */
       

    //clearBackground(g2)
    
    //no sentences? then draw no sentences message then
    if (currentCorpusNodes.size == 0) {
      drawNothingLoadedMessage(g2, metrics);
      return
    }

    
    drawCorpusIndicators(g2, metrics)
    drawSentenceIndexes(g2, metrics)
    drawSelectionWindow(g2, metrics)
    drawNavigationHandles(g2)
    //Toolkit.getDefaultToolkit().sync();
        
    println("\n\tin paintComponent: window = " + leftWindow + "/" + rightWindow + ", view = " + leftView + "/" + rightView) 
       println("3 view percent = " + viewPercent)
       println("3 zoom percent = " + zoomPercent)
      println("maxView = " + maxView) 
    
  }

  def resetShapePositions {
    setWindowShape
    setZoomShape
    setViewShape
    setSentenceShape
  }

  def positionSentenceShape(x : Int) {
    val w = 20
    val hw = w/2
    val qw = w/4
    val xp = Array[Int](x, x + w, x + w / 2 + 1, x + w / 2 + 1, x + w / 2 - 0)
    val yp = Array[Int]((size.getHeight.toInt / 2) + qw, (size.getHeight.toInt / 2) + qw, size.getHeight.toInt / 2 - hw, hw, size.getHeight.toInt / 2 - hw)
    val p = new Polygon(xp, yp, 5)
    sentenceShape = p
  }

  def setSentenceShape {
    
    if (currentSentence < leftWindow) {
      currentSentence = leftWindow 
    } else if (currentSentence > rightWindow) {
      currentSentence = rightWindow 
    }

    val w = 20
    val zx = (getExactSentencePixel(currentSentence) - (w / 2))
    //sentenceShape.setRect(zx, (getHeight / 2) - 8, w, 18)

    positionSentenceShape(zx)
  }

  def setViewShape {
    val w = 20
    val h = 10
    println("in viewShape : viewPercent = " + viewPercent)
    val zx = ((size.getWidth * viewPercent) - (w / 2)).toInt
    viewShape.setRect(zx, 2, w, h)
  }

  def setViewPercent {
    val mpix = (viewShape.getX + (viewShape.getWidth / 2)).toInt
    viewPercent = (mpix / size.getWidth).toDouble
  }

  def setZoomShape {
    val w = 40
    val h = 10
    val zx = ((size.getWidth * zoomPercent) - (w / 2)).toInt
    zoomShape.setRect(zx, size.getHeight - (h + 2), w,h)
  }

  def setZoomPercent {
    val mpix = (zoomShape.getX + (zoomShape.getWidth / 2)).toInt
    zoomPercent = (mpix / size.getWidth).toDouble
  }

  def calculateViewPercent {
    val middleView = (leftView + (viewRange / 2.0))
    viewPercent = (middleView / maxView.toDouble)
    //viewPercent = (middleView / rightView)
    println("in calculateViewPercent : viewPercent = " + viewPercent)
  }

  def calculateViewPositions {
    val middleView = (viewPercent * maxView)
    val half = (viewRange / 2.0)

    leftView = (middleView - half).toInt
    rightView = (middleView + half).toInt

    if (leftView < 0) {
      leftView = 0
      //viewRange = rightView = leftView
    }
    if (rightView > maxView) {
      rightView = maxView
      leftView = rightView - viewRange + 1
      //viewRange = rightView = leftView
    }

    //println("middleView = " + middleView + " / maxView = " + maxView)
  }
  
  def calculateViewRangeWhenDraggingWindow(origWindowSize : Int) {
    val range = (zoomPercent * (maxView.toDouble)).toInt
    rightView = leftView + range
    if (rightView > maxView) {
      leftView -= (rightView - maxView)
      rightView = maxView
    }
    viewRange = rightView - leftView + 1

    if (rightWindow > rightView) {
      rightWindow = rightView
      leftWindow = rightView - origWindowSize
    }
    
    if (leftWindow < leftView) {
      leftWindow = leftView
      rightWindow = leftView + origWindowSize
    }
    
    if (rightWindow < leftWindow) {
      leftWindow = rightWindow
    }
    
    calculateViewPercent
    setViewShape
    setSentenceShape

  }
 
   
  def calculateViewRangeWhenJogging(range: Int) {
    //val range = (zoomPercent * (maxView.toDouble)).toInt
    rightView = leftView + range
    if (rightView > maxView) {
      leftView -= (rightView - maxView)
      rightView = maxView
    }
    viewRange = rightView - leftView + 1
    //println("viewRange = " + viewRange)

    if (rightWindow > rightView) {
      rightWindow = rightView
      setRightSentenceIndex(size.getWidth.toInt)
    }
    
    if (leftWindow < leftView) {
      leftWindow = leftView
      setLeftSentenceIndex(0)
    }
    
    if (rightWindow < leftWindow) {
      leftWindow = rightWindow
    }
    

    calculateViewPercent
    setViewShape
  }

  def keepInRange {
    if (leftView < 0) {
      leftView = 0
    }
    if (rightView > maxView) {
      rightView = maxView
    }

    if (leftWindow < leftView) {
      leftWindow = leftView
    }
    if (rightWindow > rightView) {
      rightWindow = rightView
    }

    if (leftWindow == rightWindow) {
      rightWindow = leftWindow + 1
    }
    if (leftView == rightView) {
      rightView = leftView + 1
    }

    if (zoomPercent < 0.0) {
      zoomPercent = 0.0
    }
    if (zoomPercent > 1.0) {
      zoomPercent = 1.0
    }

    viewRange = rightView - leftView
  }

  def calculateViewRange {
    val range = (zoomPercent * (maxView.toDouble)).toInt
    rightView = leftView + range
    if (rightView > maxView) {
      leftView -= (rightView - maxView)
      rightView = maxView
    }
    viewRange = rightView - leftView + 1
    //println("viewRange = " + viewRange)

    if (rightWindow > rightView) {
      rightWindow = rightView
      setRightSentenceIndex(size.getWidth.toInt)
    }
    
    if (leftWindow < leftView) {
      leftWindow = leftView
      setLeftSentenceIndex(0)
    }
    
    if (rightWindow < leftWindow) {
      leftWindow = rightWindow
    }
    
    keepInRange

    calculateViewPercent
    setViewShape
  }

  def setWindowShape {
    val lp = getLeftSentencePixel(leftWindow)
    val rp = getRightSentencePixel(rightWindow)
    windowShape.setRect(lp, 10, rp - lp, (size.getHeight / 2) - (10))
    setSizerShapes
  }

  def setSizerShapes {
    setLeftSizerShape
    setRightSizerShape
  }

  def setLeftSizerShape {
    val lx = windowShape.getX.toInt
    val my = (windowShape.getY + (windowShape.getHeight / 2) - 5).toInt
    leftWindowSizerShape.setRect(lx, my, 10, 10)
  }

  def setRightSizerShape {
    val rx = (windowShape.getX + windowShape.getWidth - 10).toInt
    val my = (windowShape.getY + (windowShape.getHeight / 2) - 5).toInt
    rightWindowSizerShape.setRect(rx, my, 10, 10)
  }

  def setSentenceIndex(pix: Int) {
    val inc = (size.getWidth.toDouble / viewRange.toDouble)
    val idx = (pix) / inc
    currentSentence = (leftView + idx).toInt
  }

  def setWindowIndexes {
    setLeftSentenceIndex(windowShape.getX.toInt)
    setRightSentenceIndex( (windowShape.getX + windowShape.getWidth).toInt  )
  }



  def setLeftSentenceIndex(pix: Int) {
    val inc = (size.getWidth / viewRange.toDouble)
    val idx = (pix) / inc
    leftWindow = (leftView + idx).toInt
  }

  def setRightSentenceIndex(pix: Int) {
    val inc = (size.getWidth.toDouble / viewRange.toDouble)
    val idx = pix / inc

    // if (g_isJogging) {
      //   rightWindow = (int) (leftView + idx)
      // }

      if (windowShapeSelected || zoomShapeSelected) {
        //rightWindow = (int) math.round(leftView + idx - 1)
        rightWindow = (leftView + idx - 1).toInt

      } else {
        rightWindow = (leftView + idx).toInt
      }
    }

    def getExactSentencePixel(sidx: Int) : Int =  {
      val inc = (size.getWidth.toDouble / viewRange.toDouble)
      val margin = inc / 2.0
      return (math.round(margin + ((sidx - leftView) * inc))).toInt
    }

    def getLeftSentencePixel(sidx: Int) : Int = {
      val inc = (size.getWidth.toDouble / viewRange.toDouble)
      return  (math.round((sidx - leftView) * inc)).toInt
    }

    def getRightSentencePixel(sidx: Int) : Int = {
      val inc = (size.getWidth.toDouble / viewRange.toDouble)
      return (math.round((sidx + 1 - leftView) * inc)).toInt
    }

    def getNotchIncrement : Int = {
      var inc = 1
      if (viewRange > 5000) {
        inc = 1000
      } else if (viewRange > 3000) {
        inc = 500
      } else if (viewRange > 1500) {
        inc = 200
      } else if (viewRange > 750) {
        inc = 100
      } else if (viewRange > 250) {
        inc = 50
      } else if (viewRange > 120) {
        inc = 20
      } else if (viewRange > 75) {
        inc = 10
      } else if (viewRange > 17) {
        inc = 5
      }

      return inc
    }


    //deal with this later...
    //public void fullyExpand {}
    //public void initializeRangeSettings {}

    def initializeRangeSettings : Unit = {
      //println("in initializeRangeSettings")
      //currentCorpusNodes = Main.project.getUniqueSelectedCorpusNodes

      val tempSentenceNums = new ListBuffer[Int]

      var rightView = 0

      //println("here... currentCorpusNodes size = " + currentCorpusNodes.size)
      for (i <- 0 until currentCorpusNodes.size) {
        val cd = currentCorpusNodes(i)
        //println("CorpusData " + i + " = " + cd)

        val s = cd.rawStartIdx
        val e = cd.rawEndIdx
        //int s = 1
        //int e = 100
        val cdr = (e - s)

        cd.startIdx = (rightView)
        cd.endIdx = (rightView + cdr)

        //for (int j = s; j <= e; j++) {
          for (j <- s to e) {
            tempSentenceNums.append(j)
          }
          //println("s/e = " + cd.startIdx + "/" + cd.endIdx)

          rightView += cdr

          //if (i < ControlPanel.corpusNodes.size - 1)
          if (i < currentCorpusNodes.size - 1) {
            rightView = rightView + 1
          }

        }
        zoomPercent = .5
        viewPercent = .3
        maxView = rightView


        calculateViewRange

        //System.out.println("leftView/rightView = " + leftView + "/" + rightView)
        //System.out.println("leftWindow/rightWindow = " + leftWindow + "/" + rightWindow)

        //this.rightView = rightView
        leftWindow = (leftView + (viewRange * .25)).toInt
        rightWindow = (leftView + (viewRange * .75)).toInt

        //this.leftWindow = (int) (rightView * .25)
        //this.rightWindow = (int) (rightView * .75)


        //this.viewRange = rightView + 1
        this.windowRange = rightWindow - leftWindow
        this.currentSentence = leftWindow + (windowRange / 2)

        /*
        synchronized (Main.sentenceNums) {
          Main.sentenceNums.clear
          Main.sentenceNums.addAll(tempSentenceNums)
        }
        */

        paintNow
        //this.paintImmediately(0,0,500, 500)
        //println("out initializeRangeSettings")

        //handleDatabaseConnections(false)
      }


      def handleLeftWindowSizerDrag(x: Int) {
        val newx = checkBounds(x - offx, rightWindowSizerShape)
        val rwssd = leftWindowSizerShape.getWidth.toInt

        if (newx <= 0) {
          if (g_isJogging != true) {
            zoomUp = false
            zoomDown = true
            addJogThread; ///SHOULD BE ON
            return
          }
        } else {
          g_isJogging = false
          zoomDown = false
          zoomUp = false
        }

        if (newx > getLeftSentencePixel(rightWindow)) {
          val temp = leftWindowSizerShape
          leftWindowSizerShape = rightWindowSizerShape
          rightWindowSizerShape = temp
          leftWindowSizerShapeSelected = false
          rightWindowSizerShapeSelected = true

          leftWindowSizerShape.setRect(getLeftSentencePixel(rightWindow), leftWindowSizerShape.getY, rwssd, rwssd)
          leftWindow = rightWindow

          val tx = getLeftSentencePixel(leftWindow)
          val tw = getRightSentencePixel(leftWindow) - tx

          windowShape.setRect(tx, windowShape.getY.toInt, tw, windowShape.getHeight.toInt)
          
          //currentSentence = rightWindow
          
          //setSentenceShape
          //repaint
          return
        } else {
          val neww = (windowShape.getX + windowShape.getWidth).toInt - newx
          windowShape.setRect(newx, windowShape.getY.toInt, neww, windowShape.getHeight.toInt)
          setLeftSizerShape
          setLeftSentenceIndex(newx)

          //if (newx > sentenceShape.getX)
          if (newx > sentenceShape.getBounds.getX) {
            //sentenceShape.setLocation(newx, (int)sentenceShape.getY)
            positionSentenceShape(newx)
           
            //currentSentence = leftWindow
          }
        }
      }

      def handleRightWindowSizerDrag(x: Int) {
        val newx = checkBounds(x - offx, rightWindowSizerShape)
        val rwssd = rightWindowSizerShape.getWidth.toInt

        if (newx + rwssd >= size.getWidth) {
          if (g_isJogging != true) {
            zoomUp = true
            zoomDown = false
            addJogThread //SHOULD BE ON
            return
          }
        } else {
          g_isJogging = false
          zoomDown = false
          zoomUp = false
        }


        var neww = -1

        if (newx + rwssd < getRightSentencePixel(leftWindow)) {
          val temp = leftWindowSizerShape
          leftWindowSizerShape = rightWindowSizerShape
          rightWindowSizerShape = temp
          leftWindowSizerShapeSelected = true
          rightWindowSizerShapeSelected = false

          rightWindowSizerShape.setRect(getRightSentencePixel(leftWindow) - rwssd, rightWindowSizerShape.getY, rwssd, rwssd)
          rightWindow = leftWindow

          //neww = ((int)rightWindowSizerShape.getX + rwssd) - (int) leftWindowSizerShape.getX
          //windowShape.setRect(leftWindowSizerShape.getX, (int)windowShape.getY, neww, (int) windowShape.getHeight)

          val tx = getLeftSentencePixel(rightWindow)
          val tw = getRightSentencePixel(rightWindow) - tx

          windowShape.setRect(tx, windowShape.getY.toInt, tw, windowShape.getHeight.toInt)

          //THINK THIS THROUGH-- it is not quite right!!!
          //currentSentence = leftWindow
          setSentenceShape


          //repaint
          return

        } else {
          //10 is the width of the rightWindowSizerShape!
          rightWindowSizerShape.setRect(newx, rightWindowSizerShape.getY, rwssd, rwssd)
          neww = (newx + rwssd) - windowShape.getX.toInt
          windowShape.setRect(windowShape.getX, windowShape.getY.toInt, neww, windowShape.getHeight.toInt)
          //setRightSizerShape
          setRightSentenceIndex(newx + rwssd)
          //System.out.println("leftWindow/rightWindow = " + leftWindow + "/" + rightWindow)

          //if (windowShape.getX + windowShape.getWidth < sentenceShape.getX + sentenceShape.getWidth)
          if (windowShape.getX + windowShape.getWidth < sentenceShape.getBounds.getX + sentenceShape.getBounds.getWidth) {
            //sentenceShape.setLocation((int)(windowShape.getX + windowShape.getWidth -sentenceShape.getWidth ), (int)sentenceShape.getY)
            positionSentenceShape( (windowShape.getX + windowShape.getWidth - sentenceShape.getBounds.getWidth).toInt )
            //currentSentence = rightWindow
          }
        }

      }

      def handleWindowShapeDrag(x : Int) {
        val newx = checkBounds(x - offx, windowShape)
        //System.out.println("NEWX = " + (newx + windowShape.getWidth) + " AND getWidth = " + getWidth)
        if (newx + windowShape.getWidth >= size.getWidth) {
          if (g_isJogging != true) {
            zoomUp = true
            zoomDown = false
            addJogThread; 
            return
          }
        } else if (newx <= 0) {
          if (g_isJogging != true) {
            zoomDown = true
            zoomUp = false
            addJogThread; 
            return
          }
        } else {
          g_isJogging = false
          zoomDown = false
          zoomUp = false
        }

        //int sentencePixelDiff = (int) (sentenceShape.getX - windowShape.getX)
        val sentencePixelDiff = (sentenceShape.getBounds.getX - windowShape.getX).toInt
        val midSentDiff = currentSentence - leftWindow

        windowShape.setLocation(newx, windowShape.getY.toInt)
        //sentenceShape.setLocation(newx + sentencePixelDiff, (int) sentenceShape.getY)
        positionSentenceShape(newx + sentencePixelDiff)


        //setSentenceIndex((int) (sentenceShape.getX  ))
        //setSentenceIndex((int) (sentenceShape.getX + (sentenceShape.getWidth/2) ))
        val sentenceDiff = rightWindow - leftWindow
        setLeftSentenceIndex(windowShape.getX.toInt)
        rightWindow = leftWindow + sentenceDiff

        currentSentence = leftWindow + midSentDiff
        //setSentenceShape
        //System.out.println("leftWindow/rightWindow = " + leftWindow + "/" + rightWindow)
        setSizerShapes
      }

      def handleSentenceShapeDrag(x : Int) {
        var newx = x - offx
        if (newx + (sentenceShape.getBounds.getWidth / 2) > getExactSentencePixel(rightWindow)) {
          newx = (getExactSentencePixel(rightWindow) - (sentenceShape.getBounds.getWidth / 2)).toInt
        } else if (newx + (sentenceShape.getBounds.getWidth / 2) < getExactSentencePixel(leftWindow)) {
          newx = (getExactSentencePixel(leftWindow) - (sentenceShape.getBounds.getWidth / 2)).toInt
        }
        positionSentenceShape(newx)
        setSentenceIndex((sentenceShape.getBounds.getX + (sentenceShape.getBounds.getWidth / 2)).toInt)
      }

      def handleZoomOrWindow(x: Int, y: Int, mods:Int) {
        if (mods != 1024) //ie if any command,ctrl,shift key pressed
        {
          if (x > prevx) {
            zoomPercent += (x - prevx) / (size.getWidth) 
          }
          else if (x < prevx) {
            zoomPercent -= (prevx - x) / (size.getWidth)
          }
          if (zoomPercent < 0.0) {
            zoomPercent = 0.0
          }
          else if (zoomPercent > 1.0) {
            zoomPercent = 1.0
          }

          calculateViewRange
          setWindowShape
          if (currentSentence < leftView) {
            currentSentence = leftView
          }
          if (currentSentence > rightView) {
            currentSentence = rightView
          }
          setSentenceShape
        } else {
          val origWindowShapeSize = rightWindow - leftWindow

          if (x > prevx) {
            viewPercent += (x - prevx) / (size.getWidth*2) 
          }
          else if (x < prevx) {
            viewPercent -= (prevx - x) / (size.getWidth*2)
          }
          if (viewPercent < 0.0) {
            viewPercent = 0.0
          }
          else if (viewPercent > 1.0) {
            viewPercent = 1.0
          }

          calculateViewPositions
          calculateViewRangeWhenDraggingWindow(origWindowShapeSize)
        }

      }

      def handleZoomShapeDrag(x : Int) {
        val newx = checkBoundsZoom(x - offx)
        zoomShape.setLocation(newx, zoomShape.getY.toInt)
        setZoomPercent
        //System.out.println("zoom percent = " + zoomPercent)
        calculateViewRange

        setWindowShape

        if (currentSentence < leftView) {
          currentSentence = leftView
        }
        if (currentSentence > rightView) {
          currentSentence = rightView
        }
        setSentenceShape
      }

      def handleViewShapeDrag(x:Int) {

        val origWindowShapeSize = rightWindow - leftWindow
        val newx = checkBoundsZoom(x - offx)
        viewShape.setLocation(newx, viewShape.getY.toInt)

        setViewPercent
        calculateViewPositions
        calculateViewRangeWhenDraggingWindow(origWindowShapeSize)
      }

      def handleDrag(me: Point, mods: Int) {
        //println("mouse dragged: " + me)
        val x = me.getX.toInt
        val y = me.getY.toInt
        var newx = -1

        if (leftWindowSizerShapeSelected == true) {
          handleLeftWindowSizerDrag(x)
        } else if (rightWindowSizerShapeSelected == true) {
          handleRightWindowSizerDrag(x)
        } else if (windowShapeSelected == true) {
          handleWindowShapeDrag(x)
        } else if (sentenceShapeSelected == true) {
          handleSentenceShapeDrag(x)
        } else if (zoomShapeSelected == true) {
          handleZoomShapeDrag(x)
          //System.out.println("leftView/rightView = " + leftView + "/" + rightView)
          //System.out.println("leftWindow/rightWindow = " + leftWindow + "/" + rightWindow)
        } else if (viewShapeSelected == true) {
          handleViewShapeDrag(x)
          //System.out.println("in mouseDragged<viewShapeSelected> : leftView/rightView = " + leftView + "/" + rightView)
          //System.out.println("in mouseDragged<viewShapeSelected> : leftWindow/rightWindow = " + leftWindow + "/" + rightWindow)
        } else {
          handleZoomOrWindow(x,y,mods)
        }

        prevx = x
        prevy = y

        /*******
        if (!g_isJogging) {
          handleDatabaseConnections(true); //this should be handled in another thread maybe
        }
        ******/

        if (!g_isJogging) {
          paintNow 
        }

      }


      def checkBoundsZoom(chx: Int) : Int = {
        var checkx = chx
        if (checkx < -(zoomShape.getWidth / 2)) {
          checkx = -(zoomShape.getWidth / 2).toInt
        } else if (checkx > size.getWidth - zoomShape.getWidth / 2) {
          checkx = (size.getWidth - zoomShape.getWidth / 2).toInt
        }

        return checkx
      }

      def checkBounds(chx:Int, r:Rectangle): Int = {
        var checkx = chx
        if (checkx < 0) {
          checkx = 0
        } else if (checkx > size.getWidth - r.getWidth.toInt + 0) {
          checkx = (size.getWidth - r.getWidth).toInt + 0
        }
        return checkx
      }


      def handlePressed(me: Point, clickCount: Int) {

        requestFocus

        println("mouse pressed: " + me)
        var x = me.getX.toInt
        var y = me.getY.toInt

        if (clickCount == 2) {
          //fullyExpand
          paintNow //repaint
          System.out.println("mousePressed : clickcount = 2")
          return
        }

        if (sentenceShape.contains(x, y)) {
          sentenceShapeSelected = true
          offx = x - sentenceShape.getBounds.getX.toInt
          offy = y - sentenceShape.getBounds.getY.toInt
        } else if (leftWindowSizerShape.contains(x, y)) {
          leftWindowSizerShapeSelected = true

          offx = x - leftWindowSizerShape.getX.toInt
          offy = y - leftWindowSizerShape.getY.toInt
          if (rightWindowSizerShape.getX > size.getWidth) {
            //upJog = false
          }
        } else if (rightWindowSizerShape.contains(x, y)) {
          rightWindowSizerShapeSelected = true
          offx = x - rightWindowSizerShape.getX.toInt;
          offy = y - rightWindowSizerShape.getY.toInt;
        } else if (windowShape.contains(x, y)) {
          windowShapeSelected = true
          offx = x - windowShape.getX.toInt;
          offy = y - windowShape.getY.toInt;

          /*
          if (rightWindowSizerShape.getX > getWidth)
            {
            upJog = false
          }
          */
        } else if (zoomShape.contains(x, y)) {

          zoomShapeSelected = true
          offx = x - zoomShape.getX.toInt;
          offy = y - zoomShape.getY.toInt;
        } else if (viewShape.contains(x, y)) {
          viewShapeSelected = true
          offx = x - viewShape.getX.toInt;
          offy = y - viewShape.getY.toInt;
        }
        prevx = x
        prevy = y
      }


      def handleReleased {
        g_isJogging = false
        windowShapeSelected = false
        leftWindowSizerShapeSelected = false
        rightWindowSizerShapeSelected = false
        zoomShapeSelected = false
        viewShapeSelected = false
        sentenceShapeSelected = false
        calculateViewPercent
        /*
        setViewShape
        setWindowShape
        setZoomShape
        setSentenceShape
        */
        //resetShapePositions

        //System.out.println("currentSentence = " + currentSentence)
        //handleDatabaseConnections(false); //this should be handled in another thread maybe

        println("in handleReleased : about to repaint")
        paintNow //repaint
      }

      def calculateViewChange : Int = {
        //var change = (zoomPercent * (Main.sentenceNums.size * .005)).toInt
        var change = zoomPercent * (100 * .005)
        if (change < 1) {
          change = 1
        }
        //System.out.println("change = " + change + " , # sents = " + gSentenceNums.size)
        change.toInt
      }

      def addJogThread {
        println("IN JOG THREAD!")
        g_isJogging = true

        val t = new Thread {
          override def run {
            while (true) {
              if (g_isJogging == false) {
                return
              }


               
              var zoomInc = .01 // (viewRange.toDouble) / (maxView.toDouble * .05)
              println("zoomInc = " + zoomInc + ", viewRange = " + viewRange)
              //for sentence shape/index calculation
              val sentencePixelDiff = (sentenceShape.getBounds.getX - windowShape.getX).toInt
              val midSentDiff = currentSentence - leftWindow


              if (zoomUp && rightView < maxView) { 
                if (rightWindowSizerShapeSelected) {


                  zoomPercent = math.min(zoomPercent + zoomInc, 1.0) //will probably need to calculate the increase based on # of sentences loaded
                  calculateViewRange
                  //calculateViewRangeWhenJogging(1)
                  rightWindow = rightView
                  //setWindowShape
                }
                else {
                  //val change = calculateViewChange
                  val change = calculateViewChange
                  val prevWindowDiff = rightWindow - leftWindow
                  leftView += change

                  if (leftView > maxView - viewRange) {
                    rightView = maxView
                    leftView = rightView - (viewRange - 1)
                  }

                  setRightSentenceIndex(size.getWidth.toInt)

                  if (leftView < 0) {
                    leftView = 0
                  }


                  if (!rightWindowSizerShapeSelected) {
                    leftWindow = rightWindow - prevWindowDiff
                  }
                }
              }
              else if (zoomDown && leftView > 0) {
                if (leftWindowSizerShapeSelected) {
                  zoomPercent = math.max(zoomPercent - zoomInc, 0.0) 
                  calculateViewRange
                 // calculateViewRangeWhenJogging(-1)
                  leftWindow = leftView
                  
                  keepInRange

                  //setWindowShape
                }
                else {
                  val change = calculateViewChange
                  val prevWindowDiff = rightWindow - leftWindow
                  val prevViewDiff = rightView - leftView
                  leftView -= change
                  rightView -= change
                  if (leftView < 0) {
                    leftView = 0
                    rightView = prevViewDiff
                  }

                  setLeftSentenceIndex(windowShape.getX.toInt)
                  if (!leftWindowSizerShapeSelected) {
                    rightWindow = leftWindow + prevWindowDiff
                  }


                  if (rightView > maxView) {
                    rightView = maxView
                  }


                }
              }

              /*
              if (zoomUp && rightView < maxView) {
                val prevWindowDiff = rightWindow - leftWindow
                leftView += change

                if (leftView > maxView - viewRange) {
                  rightView = maxView
                  leftView = rightView - (viewRange - 1)
                }

                setRightSentenceIndex(size.getWidth.toInt)
                if (!rightWindowSizerShapeSelected) {
                  leftWindow = rightWindow - prevWindowDiff
                }
                //setWindowIndexes
              } else if (zoomDown && leftView > 0) {
                val prevWindowDiff = rightWindow - leftWindow

                val prevViewDiff = rightView - leftView
                leftView -= change
                rightView -= change
                if (leftView < 0) {
                  leftView = 0
                  rightView = prevViewDiff
                }

                setLeftSentenceIndex(windowShape.getX.toInt)
                if (!leftWindowSizerShapeSelected) {
                  rightWindow = leftWindow + prevWindowDiff
                }
              }
              */
              //calculateSentenceIndexes
              //calculateViewIndexes
              //calculateWindowIndexes

              //System.out.println("JOG 2 : sentenceIndex_a = " + leftView)
              //System.out.println("JOG 2 : sentenceIndex_b = " + rightView)
              //System.out.println("JOG 2: g_sentenceRange = " + viewRange)

              /*
              if (zoomUp == true)
                {
                val prevSentenceDiff = currentSentence - leftWindow
                rightWindow = rightView
                leftWindow = rightWindow - windowRange
                currentSentence = leftWindow + prevSentenceDiff
              }
              else if (zoomDown)
                {
                val prevSentenceDiff = currentSentence - leftWindow

                leftWindow = leftView
                rightWindow = leftView + windowRange
                currentSentence = leftWindow + prevSentenceDiff

              }
              */

              //System.out.println("JOG 3 : sentenceIndex_a = " + leftView)
              //System.out.println("JOG 3 : sentenceIndex_b = " + rightView)
              //System.out.println("JOG 3: g_sentenceRange = " + viewRange)


              //System.out.println("J leftView/rightView = " + leftView + "/" + rightView)
              //System.out.println("J leftWindow/rightWindow = " + leftWindow + "/" + rightWindow)

              calculateViewPercent
              setViewShape


              //now update sentence position and index
              //sentenceShape.setLocation((int)(windowShape.getX + sentencePixelDiff), (int) sentenceShape.getY)
              positionSentenceShape((windowShape.getX + sentencePixelDiff).toInt)
              currentSentence = leftWindow + midSentDiff


              //System.out.println("in JogThread : leftWindow / rightWindow = " + leftWindow + "/" + rightWindow)
              if (numThreads > 0) {
                //handleDatabaseConnections(true); //yes we are dragging...
              }

              paintNow //repaint 

              try {
                Thread.sleep(20)
                //println("SLEEP FOR " + sleepy)
                //Thread.sleep(sleepy)
              } catch {
                case ex:Exception =>  
              }

              //Runnable updateAComponent = new Runnable
              //{ public void run { repaint; } }
              //SwingUtilities.invokeLater(updateAComponent)
              //System.out.println("NUM THREADS = " + numThreads)

              //repaint

              //long end_time = System.currentTimeMillis

              //System.out.println("time = " + (end_time - st_time))
            }
          }
        }

        t.start
      }


      listenTo(this.mouse.clicks, this.mouse.moves, this.keys)
      reactions += {
        case MouseDragged(src, point, mods) ⇒ {handleDrag(point, mods); println(mods) }
        case MousePressed(src, point, i1, i2, b) ⇒  {handlePressed(point, i2); println(i1 + "," + i2 + "," + b);}
        case MouseReleased(src,  point, i1, i2, b) => handleReleased
        case KeyPressed(src, key, modifiers, location) ⇒ Main.handleKeyPress(key, modifiers)
      }

    }

