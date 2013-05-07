package gui


import swing._ //{ MainPanel, Panel, Component, LayoutContainer, Orientation }
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
import javax.swing.SwingConstants
import CorpusData._
import corpus2._
import corpus2.Driver._

//need to make sure mouse stuff doesn't happen if NO corpora attached to navigate panel...
class NavigatePanel extends Panel{

  //the current selected view of the window
  var leftView = 0
  var rightView = 0
  //the current visible window into the full range of corpus sentences
  var leftWindow = 0
  var rightWindow = 0
  //the full range of sentences loaded in from the corpus tree
  var leftRange = 0
  var rightRange = 0

  var numSentencesInCurrentView = 0
  var numSentencesInCurrentWindow = 0
  var numSentencesInCurrentRange = 0
  var currentSentence = -1 

  //shapes
  var windowShape = new Rectangle(0, 0, 0, 0)
  var leftWindowSizerShape = new Rectangle(0, 0, 0, 0)
  var rightWindowSizerShape = new Rectangle(0, 0, 0, 0)
  var sentenceShape = new Polygon;

  //shape selection
  var windowShapeSelected = false
  var leftWindowSizerShapeSelected = false
  var rightWindowSizerShapeSelected = false
  var sentenceShapeSelected = false
  var isJogging = false
  var isJoggingLeft = false
  var isJoggingRight = false

  //previous mouse positions
  var prevx = 0
  var prevy = 0

  //offsets for dragging
  var offx = 0
  var offy = 0

  //info for expanding and unexpanding
  var isExpanded = false
  var unexpandedLeftWindow = -1 
  var unexpandedRightWindow = -1 
  var unexpandedLeftView = -1 
  var unexpandedRightView = -1 
  var prevLV = -1
  var prevRV = -1
  var prevLW = -1
  var prevRW = -1
  
  var prevS = -1;

  //margins not used yet...
  var topMargin = 30
  var bottomMargin = 30
  var leftMargin = 0
  var rightMargin = 0

  val SENTENCE_SHAPE_WIDTH = 20
  val SENTENCE_SHAPE_HEIGHT = 10
  val SENTENCE_SHAPE_POINTER_PERCENT = .48
  val CORPUS_INDICATOR_FONT = new Font("Menlo", Font.PLAIN, 12)

  var CORPUS_POS = -1
  var NOTCH_POS_1 = -1
  var NOTCH_POS_2 = -1
  var WINDOW_POS = -1

  //opaque = true
  //peer.setOpaque(true)
  //peer.setDoubleBuffered(true)
  //peer.setIgnoreRepaint(true);
  focusable=true
  border=null

  var rm = RepaintManager.currentManager(this.peer);
  rm.setDoubleBufferingEnabled(true);

  
  listenTo(this, this.mouse.clicks, this.mouse.moves, this.keys)
  reactions += {
    case scala.swing.event.ComponentResized(src) => {
      //display.preferredSize = (size.width, TOP_MARGIN + HEADER_HEIGHT + (biggestSize * COLOCATION_HEIGHT) + BOTTOM_MARGIN)
      paintNow
    }
    case MouseDragged(src, point, mods) ⇒ {handleDrag(point, mods)}
    case MousePressed(src, point, i1, i2, b) ⇒  {handlePressed(point, i1, i2); println(i1 + "," + i2 + "," + b);}
    case MouseReleased(src,  point, i1, i2, b) => handleReleased
    case KeyPressed(src, key, modifiers, location) ⇒ this.handleKeyPress(key, modifiers)
    case KeyReleased(src, key, modifiers, location) ⇒ this.handleKeyRelease(key, modifiers)
    case _ =>
  }

  initializeRangeSettings


  def initializeRangeSettings {
    val tempSentenceNums = new ListBuffer[Int]
    var currentIdx = 0

    for (i <- 0 until currentCorpusNodes.size) {
      val cd = currentCorpusNodes(i)

      val s = cd.rawStartIdx
      val e = cd.rawEndIdx
      val corpusRange = (e - s) 

      cd.startIdx = (currentIdx)
      cd.endIdx = (currentIdx + corpusRange)
      
      currentIdx += corpusRange

      if (i < currentCorpusNodes.size - 1) {
        currentIdx = currentIdx + 1
      }

      println("CORPUS DATA " + cd.name + " (" + cd.startIdx + "/" + cd.endIdx + ")")
    }

    leftRange = 0
    rightRange = currentIdx
    numSentencesInCurrentRange = rightRange - leftRange

    leftView = leftRange + (numSentencesInCurrentRange * .25).toInt
    rightView = leftRange + (numSentencesInCurrentRange * .75).toInt
    numSentencesInCurrentView = rightView - leftView

    leftWindow = leftView + (numSentencesInCurrentView * .25).toInt
    rightWindow = leftView + (numSentencesInCurrentView * .75).toInt
    numSentencesInCurrentWindow = rightWindow - leftWindow 

    println("currentCorpusNodes size = " + currentCorpusNodes.size)
    if (currentCorpusNodes.size > 0) {
      currentSentence = leftWindow + (numSentencesInCurrentWindow / 2)
      keepInRange
    } else {
      currentSentence = -1
    }

    paintNow

    //println("about to call newSentence with val of " + currentSentence)
    if (currentCorpusNodes.size > 0) {
      updateWidgets(true, true)
    } else {
      Main.currentView.clearPanels
    }


    //Main.sentencePanel.newSentence(currentSentence)
    //handleDatabaseConnections(false)
  }


  def clearBackground(g2: Graphics2D) {
    g2.setColor(Color.WHITE)
    g2.fillRect(0, 0, size.getWidth.toInt, size.getHeight.toInt)
  }

  def getFontMetrics(g2: Graphics2D) : FontMetrics = {
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2.setFont(CORPUS_INDICATOR_FONT)
    g2.getFontMetrics
  }

  def drawNothingLoaded(g2: Graphics2D, metrics: FontMetrics) {
    g2.setColor(Color.GRAY)
    g2.fillRect(0, 0, size.getWidth.toInt, size.getHeight.toInt)
  }

  def drawCorpusIndicators(g2: Graphics2D, metrics: FontMetrics) {
    g2.setFont(CORPUS_INDICATOR_FONT)

    for {cd <- currentCorpusNodes 
    spix = getLeftSentencePixel(cd.startIdx)
    epix = getRightSentencePixel(cd.endIdx)
    if (epix >= 0 && spix <= size.getWidth) } {

      g2.setColor(cd.color)
      g2.fillRect(spix, 0, epix - spix, size.getHeight.toInt)
      g2.setColor(Color.GRAY)
      if (spix > 0) {
        g2.drawLine(spix, 0, spix, size.getHeight.toInt)
      }
      if (epix < size.getWidth) {
        g2.drawLine(epix, 0, epix, size.getHeight.toInt)
      }
      g2.setColor(Color.BLACK)

      if (metrics.stringWidth(cd.name) + 10 < (epix - spix)) {
        g2.drawString(cd.name, spix + ((epix - spix) / 2) - (metrics.stringWidth(cd.name).toInt / 2), CORPUS_POS)
      }
    }
  }

  def drawSentenceIndexes(g2: Graphics2D, metrics: FontMetrics ) {
    val inc = getNotchIncrement
    for (i <- (leftView - inc) to (rightView + inc); notchPix = getCenterSentencePixel(i); if (i % inc == 0)) {
      g2.setColor(Color.BLACK)
      val notchWidth = metrics.stringWidth("" + i)
      g2.setFont(CORPUS_INDICATOR_FONT)
      g2.drawString("" + i, notchPix - (notchWidth / 2), NOTCH_POS_1)
      g2.drawRect(notchPix, NOTCH_POS_2 - 8, 0, 9)
    }
  }

  def drawSelectionWindow(g2: Graphics2D, metrics: FontMetrics ) {
    g2.setColor(new Color(255, 255, 255, 194))
    g2.fill(windowShape)

    val numS = "" + (numSentencesInCurrentWindow)
    val numSW = metrics.stringWidth(numS)
    val numSH = metrics.getHeight

    g2.setColor(Color.BLACK)
    g2.drawString(numS, 
      (windowShape.getX + (windowShape.getWidth / 2) - (numSW / 2)).toInt, 
      (windowShape.getY + (windowShape.getHeight / 2) + (numSH / 2)).toInt)

    g2.setColor(Color.BLACK)
    g2.draw(windowShape)

    //draw sizer handles
    g2.setColor(Color.BLACK)
    g2.fill(leftWindowSizerShape)
    g2.fill(rightWindowSizerShape)

    //draw sentence selector
    g2.setColor(Color.BLACK)
    g2.fill(sentenceShape)
  }

  def debugRange = {
    println("Range: " + leftRange + "/" + rightRange + ", numSentencesInCurrentRange = " + numSentencesInCurrentRange)
    println("\tView: " + leftView + "/" + rightView + ", numSentencesInCurrentView = " + numSentencesInCurrentView)
    println("\t\tWindow: " + leftWindow + "/" + rightWindow + ", numSentencesInCurrentWindow = " + numSentencesInCurrentWindow)
    println("\t\t\tSentence: " + currentSentence)
  }

  def paintNow() {
    rm.markCompletelyDirty(this.peer)
    repaint
  }

  def calculateComponentPositions(metrics: FontMetrics) {
    CORPUS_POS = size.getHeight.toInt - 1
    NOTCH_POS_1 = CORPUS_POS - metrics.getHeight - 3
    NOTCH_POS_2 = NOTCH_POS_1 - metrics.getHeight - 1
    WINDOW_POS = NOTCH_POS_2 - 5
  }

  override def paintComponent(g2: Graphics2D) {

    //debugRange

    setWindowShape
    setSentenceShape

    val metrics = getFontMetrics(g2)
    calculateComponentPositions(metrics)

    if (currentCorpusNodes.size == 0) {
      drawNothingLoaded(g2, metrics);
    } else {
      drawCorpusIndicators(g2, metrics)
      drawSelectionWindow(g2, metrics)
      drawSentenceIndexes(g2, metrics)
      Toolkit.getDefaultToolkit().sync();
    }
  }

  def setWindowShape {
    var lp = getLeftSentencePixel(leftWindow)
    var rp = getRightSentencePixel(rightWindow)

    windowShape.setRect(lp, 2, rp - lp, WINDOW_POS)
    val lx = windowShape.getX.toInt
    val ly = (windowShape.getY + (windowShape.getHeight / 2) - 5).toInt
    leftWindowSizerShape.setRect(lx, ly, 10, 10)
    val rx = (windowShape.getX + windowShape.getWidth - 10).toInt
    val ry = (windowShape.getY + (windowShape.getHeight / 2) - 5).toInt
    rightWindowSizerShape.setRect(rx, ry, 10, 10)
  }

  def setSentenceShape {
    if (currentSentence < leftWindow) {
      currentSentence = leftWindow 
    } else if (currentSentence > rightWindow) {
      currentSentence = rightWindow 
    }

    val x = (getCenterSentencePixel(currentSentence) - (SENTENCE_SHAPE_WIDTH / 2))
    val w = SENTENCE_SHAPE_WIDTH 
    val h = SENTENCE_SHAPE_HEIGHT
    val ptr = SENTENCE_SHAPE_POINTER_PERCENT
    val hw = w/2
    val qw = w/4
    val xp = Array[Int](x, x + w, x + w / 2 + 1, x + w / 2 + 1, x + w / 2 - 0)
    val yp = Array[Int](0, 0, h, (size.getHeight*SENTENCE_SHAPE_POINTER_PERCENT).toInt, h)
    val p = new Polygon(xp, yp, 5)
    sentenceShape = p
  }

  def keepInRange {
    checkView
    checkWindow
    checkSentence

    def checkView {
      if (leftView < leftRange) {
        leftView = leftRange
      }
      if (rightView < leftRange) {
        rightView = leftRange 
      } 
      if (rightView > rightRange) {
        rightView = rightRange
      }
      if (leftView > rightRange) {
        leftView = rightRange
      }

      numSentencesInCurrentView = (rightView - leftView) + 1
    }

    def checkWindow {
      if (leftWindow < leftView) {
        leftWindow = leftView
      }
      if (rightWindow < leftView) {
        rightWindow = leftView
      }
      if (rightWindow > rightView) {
        rightWindow = rightView
      }
      if (leftWindow > rightView) {
        leftWindow = rightView
      }

      numSentencesInCurrentWindow = (rightWindow - leftWindow) + 1
    }

    def checkSentence {
      if (currentSentence < leftWindow) {
        currentSentence = leftWindow
      }
      if (currentSentence > rightWindow) {
        currentSentence = rightWindow
      }
    }
  }

  def getSentenceForPixel(pix : Int) : Int = {
    (math.round(leftView + (numSentencesInCurrentView * pix/size.width))).toInt 
  }

  def getSentenceIncrementForCurrentView : Double = {
    size.getWidth.toDouble / numSentencesInCurrentView.toDouble
  }

  def getPixelForSentence(sidx: Int, perc: Double) : Int = {
    val inc = getSentenceIncrementForCurrentView
    val pix = (perc*inc) + ((sidx - leftView) * inc)
    math.round(pix).toInt
  }
  def getCenterSentencePixel(sidx: Int) : Int =  {
    getPixelForSentence(sidx, .5)
  }

  def getLeftSentencePixel(sidx: Int) : Int = {
    getPixelForSentence(sidx, 0.0)
  }

  def getRightSentencePixel(sidx: Int) : Int = {
    getPixelForSentence(sidx, 1.0)
  }


  //deal with this later...
  //public void fullyExpand {}

  def swapLeftAndRightWindowSizers() {
    //swap window sides -- for when dragging handles
    if (leftWindow >= rightWindow) {
      val tmp = rightWindow
      rightWindow = leftWindow
      leftWindow = tmp

      if (rightWindowSizerShapeSelected) {
        leftWindowSizerShapeSelected = true
        rightWindowSizerShapeSelected = false
      } else if (leftWindowSizerShapeSelected) {
        leftWindowSizerShapeSelected = false
        rightWindowSizerShapeSelected = true
      }
    }
  }

  def handleRightWindowSizerDrag(x: Int) {
    if (x < prevx && isJoggingRight) {
      killJog
    } else if (x > size.getWidth && x > prevx && !isJogging) {
      addJogThread(-1, true, false);
    } else {
      rightWindow = getSentenceForPixel((x - offx) + rightWindowSizerShape.getWidth.toInt)
      swapLeftAndRightWindowSizers 
    }
  }

  def handleLeftWindowSizerDrag(x: Int) {
    if (x > prevx && isJoggingLeft) {
      killJog
    } else if (x < 0 && x < prevx && !isJogging) {
      addJogThread(-1, false, true);
    } else {
      leftWindow = getSentenceForPixel(x - offx)
      swapLeftAndRightWindowSizers 
    }
  }

  def killJog() {
    isJoggingLeft = false 
    isJoggingRight = false
    isJogging = false
  }

  def handleWindowShapeDrag(x : Int) {
    var origWindowSize = numSentencesInCurrentWindow - 1

    if (x > prevx) {
      if (isJogging) {
        if (isJoggingLeft) {
          killJog()  
        }
      } else if (rightWindow >= rightView && ((numSentencesInCurrentView > 5) ||
        (numSentencesInCurrentView <= 15 && x > size.getWidth - 10))) { 
        addJogThread(origWindowSize, true, false) 
      } else {
        slideWindowToPixel(x - offx)
      }
    } else if (x < prevx) {
      if (isJogging) {
        if (isJoggingRight) {
          killJog
        }
      } else if (leftWindow <= leftView && ((numSentencesInCurrentView > 5) ||
        (numSentencesInCurrentView <= 15 && x < 10))) { 
        addJogThread(origWindowSize, false, true)
      } else {
        slideWindowToPixel(x - offx)
      }
    }

    def slideWindowToPixel(xp: Int) {
      var idx = getSentenceForPixel(xp)
      if (idx < leftView) {
        idx = leftView
      }

      if (idx > rightView - origWindowSize) {
        idx = rightView - origWindowSize
      }

      leftWindow = idx
      rightWindow = idx + origWindowSize
    }
  }

  def moveWindow(inc: Int) {
    var origWindowSize = numSentencesInCurrentWindow - 1
    var origViewSize = numSentencesInCurrentView - 1

    if (inc > 0) {
      rightWindow += inc
      if (rightWindow > rightRange) {
        rightWindow = rightRange
      }
      leftWindow = rightWindow - origWindowSize

      if (rightWindow > rightView) {
        rightView = rightWindow
        leftView = rightView - origViewSize
      }
    } else if (inc < 0) {
      leftWindow += inc
      if (leftWindow < leftRange) {
        leftWindow = leftRange
      }
      rightWindow = leftWindow + origWindowSize

      if (leftWindow < leftView) {
        leftView = leftWindow
        rightView = leftView + origViewSize
      }
    }
  }

  def handleSentenceShapeDrag(x : Int) {
    var newx = x - offx
    val sw = SENTENCE_SHAPE_WIDTH/2
    if (newx + sw > getCenterSentencePixel(rightWindow)) {
      newx = (getCenterSentencePixel(rightWindow) - sw).toInt
    } else if (newx + sw < getCenterSentencePixel(leftWindow)) {
      newx = (getCenterSentencePixel(leftWindow) - sw).toInt
    }
    currentSentence = getSentenceForPixel(newx + sw) 
  }

  def getNotchIncrement : Int = {
    if (numSentencesInCurrentView > 5000) 1000 
    else if (numSentencesInCurrentView > 3000) 500  
    else if (numSentencesInCurrentView > 1500) 250
    else if (numSentencesInCurrentView > 750) 100
    else if (numSentencesInCurrentView > 250) 50
    else if (numSentencesInCurrentView > 120) 25
    else if (numSentencesInCurrentView > 75) 10
    else if (numSentencesInCurrentView > 17) 5
    else 1 
  }

  def getViewIncrement() : Int = {
    if (numSentencesInCurrentView < 50) 1
    else if (numSentencesInCurrentView < 100) 3
    else if (numSentencesInCurrentView < 300) 5
    else if (numSentencesInCurrentView < 500) 8
    else 10
  }

  def getZoomIncrement() : Int = {
    if (numSentencesInCurrentView < 2) 0
    else if (numSentencesInCurrentView < 20) 1
    else numSentencesInCurrentView/20
  }

  def zoomView(inc: Int) {
    leftView -= inc
    rightView += inc

    //this can happen when shrinking the view
    if (leftView > rightView) {       
      leftView = rightView
    }
  }

  def moveSentence(inc: Int) {
    currentSentence += inc
    if (currentSentence > rightWindow) {
      rightWindow = currentSentence
    } else if (currentSentence < leftWindow) {
      leftWindow = currentSentence
    }
    
    if (rightWindow > rightView) {
      rightView = rightWindow
    } else if (leftWindow < leftView) {
      leftView = leftWindow
    }
    
  }

  def handleZoomDrag(x: Int) {
    if (x > prevx) {
      zoomView(math.max(1, getZoomIncrement))
    } else {
      zoomView(-getZoomIncrement)
    }
  }

  def handleViewDrag(x:Int) {
    if (x > prevx) {
      moveView(getViewIncrement)
    }
    else if (x < prevx) {
      moveView(-getViewIncrement) 
    }
  }


  def moveView(inc: Int) {
    val origWindowSize = numSentencesInCurrentWindow - 1
    if (inc < 0)
      {
      if (leftView + inc < 0) {
        rightView += leftView
        leftView = 0
      } else {
        leftView += inc
        rightView += inc 
      }
      keepInRange
      leftWindow = rightWindow - origWindowSize
    }
    else if (inc > 0) {
      if (rightView + inc >= rightRange) {
        leftView += rightRange - rightView
        rightView = rightRange
      } else {
        leftView += inc
        rightView += inc
      }
      keepInRange
      rightWindow = leftWindow + origWindowSize
    }
  }

  def getCorpusDataForSentence(sidx : Int) : CorpusData =  {
    //println("in getCorpusDataForSentence, what corpus is sentenceIdx " + sidx + " part of?") 
    for (i <- 0 until currentCorpusNodes.size) {
      val cd = currentCorpusNodes(i)

      if (sidx >= cd.startIdx && sidx <= cd.endIdx) {
        //println("sentence idx " + sidx + " is part of the corpus " + cd.name)
        return cd
      }

    }

    return null
  }

  def getCurrentWindowRawIndexes : List[Int] = {
    val rawIdxs = for (i <- leftWindow to rightWindow; cd = getCorpusDataForSentence(i)) yield {
      cd.idxToRawIdx(i)
    }
    rawIdxs.toList
  }

  def debugCurrentWindow {
    //println("\n in debugCurrentWindow")
    for (i <- leftWindow to rightWindow) {
      val cd = getCorpusDataForSentence(i)
      val rawIdx = cd.idxToRawIdx(i)

      println("\trawIdx = " + rawIdx + " --> globalIdx " + cd.document.sentences(rawIdx)) 

    }
  }

  def debugCurrentSentence {
    val currentCorpusData = getCorpusDataForSentence(currentSentence)
    println("currentCorpusData = " + currentCorpusData)
    val rawIdx = currentCorpusData.idxToRawIdx(currentSentence)

    print("rawIdx = " + rawIdx + " ... ") 
    println(currentCorpusData.document.sentences(rawIdx))
    println(Driver.sentences(rawIdx))

  }

  def unexpandWindow {
    leftWindow = unexpandedLeftWindow
    rightWindow = unexpandedRightWindow
    leftView = unexpandedLeftView
    rightView = unexpandedRightView
    isExpanded = false
  }

  def storeUnexpandedIdxs {
    unexpandedLeftWindow = leftWindow
    unexpandedRightWindow = rightWindow
    unexpandedLeftView = leftView
    unexpandedRightView = rightView
    isExpanded = true
  }

  def expandViewToRange() {
    if (isExpanded) {
      unexpandWindow
    } else {
      storeUnexpandedIdxs
      leftView = leftRange
      rightView = rightRange
      leftWindow = leftRange
      rightWindow = rightRange
    }
  }


  def expandViewToCorpus(idx: Int) {
    if (isExpanded) {
      unexpandWindow
    } else {
      for {cd <- currentCorpusNodes 
      if (idx >= cd.startIdx && idx <= cd.endIdx) } {
        storeUnexpandedIdxs
        leftView = cd.startIdx
        rightView = cd.endIdx
        leftWindow = leftView
        rightWindow = rightView
      }
    }
  }

  def expandWindowToView {
    if (isExpanded) {
      unexpandWindow
    } else {
      storeUnexpandedIdxs

      leftWindow = leftView
      rightWindow = rightView
    }
  }

  def updateWidgets(updateOnMove:Boolean, updateOnRelease:Boolean) {
    if (currentCorpusNodes.size > 0) {
      if (checkIfWindowIdxsChanged || updateOnRelease) {
        Main.currentView.updatePatternPanels(getCurrentWindowRawIndexes, updateOnMove, updateOnRelease)
      }

      if (checkIfSentenceIdxChanged || updateOnRelease) {
        Main.currentView.updateSentencePanels(currentSentence, updateOnMove, updateOnRelease)
        //  debugCurrentSentence
      }
    } 
  }

  def handleDrag(me: Point, mods: Int) {

    storePreviousIdxs

    //println("mouse dragged: " + me)
    val x = me.getX.toInt
    val y = me.getY.toInt

    if (leftWindowSizerShapeSelected == true) {
      handleLeftWindowSizerDrag(x)
    } else if (rightWindowSizerShapeSelected == true) {
      handleRightWindowSizerDrag(x)
    } else if (windowShapeSelected == true) {
      handleWindowShapeDrag(x)
    } else if (sentenceShapeSelected == true) {
      handleSentenceShapeDrag(x)
    } else {
      if (mods != 1024) {
        handleZoomDrag(x)
      } else {
        handleViewDrag(x)
      }
    }

    prevx = x
    prevy = y

    if (!isJogging) /*otherwise this is handled in the jog thread*/ {
      processNavigationChange
    }
  }

  def handlePressed(me: Point, modifiers: Int, clickCount: Int) {
    requestFocus
    //println("mouse pressed: " + me)
    var x = me.getX.toInt
    var y = me.getY.toInt

    if (clickCount % 2 == 0) {
      System.out.println("mousePressed : clickcount = 2")
      if (windowShape.contains(x, y)) {
        expandWindowToView
      } else {
        if (modifiers == 1024) {
          expandViewToCorpus(getSentenceForPixel(x))
        } else {
          expandViewToRange()
        }
      }
    } else {
      if (sentenceShape.contains(x, y)) {
        sentenceShapeSelected = true
        offx = x - sentenceShape.getBounds.getX.toInt
        offy = y - sentenceShape.getBounds.getY.toInt
      } else if (leftWindowSizerShape.contains(x, y)) {
        leftWindowSizerShapeSelected = true
        offx = x - leftWindowSizerShape.getX.toInt
        offy = y - leftWindowSizerShape.getY.toInt
      } else if (rightWindowSizerShape.contains(x, y)) {
        rightWindowSizerShapeSelected = true
        offx = x - rightWindowSizerShape.getX.toInt;
        offy = y - rightWindowSizerShape.getY.toInt;
      } else if (windowShape.contains(x, y)) {
        windowShapeSelected = true
        offx = x - windowShape.getX.toInt;
        offy = y - windowShape.getY.toInt;
      }
    }

    prevx = x
    prevy = y

    processNavigationChange
  }


  def handleReleased {
    killJog
    windowShapeSelected = false
    leftWindowSizerShapeSelected = false
    rightWindowSizerShapeSelected = false
    sentenceShapeSelected = false
  
    updateWidgets(false, true)
  }

  def processNavigationChange {
    keepInRange
    checkIfIdxsChanged
    updateWidgets(true, false)
    paintNow 
  }

  def storePreviousIdxs {
    prevLV = leftView
    prevRV = rightView
    prevLW = leftWindow
    prevRW = rightWindow
  }

  def checkIfSentenceIdxChanged() : Boolean = {
    if (prevS != currentSentence) {
      prevS = currentSentence
      true
    } else false
  }

  def checkIfWindowIdxsChanged() : Boolean = {
    if (prevLW != leftWindow || prevRW != rightWindow) {
      prevLW = leftWindow
      prevRW = rightWindow
      true
    } else false 
  }

  def checkIfViewIdxsChanged() : Boolean = {
    if (prevLV != leftView || prevRV != rightView) {
      prevLW = leftView
      prevRW = rightView
      true
    } else false
  }

  def checkIfIdxsChanged {
    if ( isExpanded && (prevLV != leftView || prevRV != rightView || prevLW != leftWindow || prevRW != rightWindow) ) { 
      println("Indexes have changed!");
      isExpanded = false 
    }
  }

  def handleKeyRelease(key: Value, modifiers: Int) {
    updateWidgets(true, true)  
  }

  def handleKeyPress(key: Value, modifiers: Int) {

    storePreviousIdxs

    key match {
      case Key.Right => moveWindow(getViewIncrement)
      case Key.Left => moveWindow(-getViewIncrement)
      case Key.Up => zoomView(math.max(1, getZoomIncrement))
      case Key.Down => zoomView(-getZoomIncrement)
      case Key.Comma => moveSentence(-1)
      case Key.Period => moveSentence(+1)

      case _ =>
    }
    Main.handleKeyPress(key, modifiers)

    processNavigationChange
  }

  def addJogThread(origWindowSize:Int, up:Boolean, down:Boolean) {
    isJogging = true
    isJoggingRight = up
    isJoggingLeft = down

    val t = new Thread {
      override def run {
        while (true) {
          if (isJogging == false) {
            return
          }

          if (isJoggingRight && rightView < rightRange) { 
            if (rightWindowSizerShapeSelected) {
              rightView += getViewIncrement
              rightWindow = rightView
            } else if (windowShapeSelected) {
              leftView += getViewIncrement
              rightView += getViewIncrement
              rightWindow = rightView
              keepInRange
              leftWindow = rightWindow - origWindowSize
            }
          }
          else if (isJoggingLeft && leftView > 0) {
            if (leftWindowSizerShapeSelected) {
              leftView -= getViewIncrement
              leftWindow = leftView
            } else if (windowShapeSelected) {
              leftView -= getViewIncrement
              rightView -= getViewIncrement
              leftWindow = leftView
              rightWindow = leftWindow + origWindowSize
            }
          }

          //handleDatabaseConnections(true); //yes we are dragging...

          processNavigationChange


          try {
            Thread.sleep(20)
          } catch {
            case ex:Exception =>  
          }

        }
      }
    }

    t.start
  }


}
