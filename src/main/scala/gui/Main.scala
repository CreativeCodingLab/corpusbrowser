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

object Main extends SimpleSwingApplication {

   Driver.loadBNCTest


  val sentencePanels = new ListBuffer[SentencePanel] //all of these need to be updated when the navigate panel current sentence changes
  val patternPanels = new ListBuffer[PatternPanel] //all of these need to be updated when the navigate panel view changes

  val navigatePanel = new NavigatePanel

  //start with this one for testing
  val dispersionPanel = new DispersionPanel
  patternPanels += dispersionPanel
  //dispersionPanel.patterns += new WordPattern("answer")
  //dispersionPanel.patterns += new CleanSentenceRegexPattern("""(?i)aware of.*not""".r)
  dispersionPanel.patterns += new CleanSentenceRegexPattern("""hat are the dimen""".r)
  //dispersionPanel.patterns += new WordRegexPattern("""^t.*t$""")

  val sentencePanel = new SentencePanel
  sentencePanels += sentencePanel

  def updateSentencePanels(sIdx: Int) {
    //right now there is only one kind of SentencePanel, but in the future we will have other displays
    
    for (sp <- sentencePanels) sp.newSentence(sIdx)
  }

  def updatePatternPanels(sIdxs: List[Int])  {
    for (pp <- patternPanels) pp.updatePanel(sIdxs)
  }

  implicit def tuple2Dimension(tuple: Tuple2[Int, Int]) = new Dimension(tuple._1, tuple._2)
  implicit def tuple2Point(tuple: Tuple2[Int, Int]) = new Point(tuple._1, tuple._2)


  //Driver.loadMICASETest
   //makeDummyCorpusDatas
  //currentCorpusNodes foreach println
  val rootNode = makeCorpusNodes(corpora(0))

  val topBottomPane = new TopBottomPane();
  val leftRightPane = new LeftRightPane(Orientation.Vertical, new TreeScrollPane(new TreePanel()), topBottomPane)

  val top = new MainFrame {
    title = "Corpus Browser 2.0"
    contents = leftRightPane

    System.setProperty("apple.laf.useScreenMenuBar", "true");
    menuBar = new CorpusMenuBar

    location = (400,0)
    size = (500, 500)
    open

    topBottomPane.dividerLocation=.8
    leftRightPane.dividerLocation=.3

 }

  def handleKeyPress(key: Value, modifiers: Int) {
    println("you pressed " + key)

    key match {
      case Key.Escape ⇒ {Main.exitProgram;}
      case _ => //println("modifiers = " + modifiers)
    }

    //Shift = 64
    //Control = 128
    //Command = 256
    //Option = 512
  }

  def exitProgram {
    println("we are closing the application!")
    top.closeOperation
  }

  class TreeScrollPane(c : Component) extends ScrollPane(c) {
    border=null
  }

  class TopBottomPane(o : Orientation.Value, one: Component, two: Component) extends SplitPane(o, one, two) {
    def this() = this(Orientation.Horizontal, new MyIDP(), navigatePanel);

    //oneTouchExpandable=true
    dividerSize=3
    background=new Color(0,0,0)
    border=null//new LineBorder(new Color(0,0,0), 1)
  }

  class LeftRightPane(o : Orientation.Value, one: Component, two: Component) extends SplitPane(o, one, two) {
    //peer.setDoubleBuffered(true);
    //def this() = this(Orientation.Vertical, new TestPanel(), new TestPanel()); 
    background=new Color(0,0,0)
    dividerSize=3
    border=new LineBorder(new Color(0,0,0), 2)


  }

  class MyIF(title:String, x:Int, y:Int, pw:Int, ph:Int, mw:Int, mh:Int, thepanel:Panel) extends InternalFrame {
    implicit def tuple2Point(tuple: Tuple2[Int, Int]) = new Point(tuple._1, tuple._2)

    super.title = title
    //border = Swing.LineBorder(Color.WHITE, 2)
    bounds = (x,y,pw,ph)

    contents = thepanel ; //new NavigatePanel

    minimumSize = (mw, mh)
    preferredSize = (pw, ph)

    iconifiable = true
    closable = true
    maximizable = true
    resizable = true
    visible = true


  }

  class MyIDP() extends InternalDesktopPane {

    dragMode = DragMode.Live //Outline
    background= new Color(255,255,255)
    var if1 = new MyIF("dispersion", 100, 10, 300,200,250, 100, dispersionPanel)
    add(if1)
    
    var if2 = new MyIF("sentence", 130, 20, 300,200,250, 100, sentencePanel)
    add(if2)

    def add(ifr: InternalFrame) {
      super.add(ifr, new LayerConstraints)
    }

  }










  //just testing out migpanel stuff below here...
  class MyBorderPanel extends MigPanel("wrap 2") {
    val but = new Button {
      text = "Click me"
    }
    val but2 = new Button {
      text = "Click me!"
    }
    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.setColor(new Color(100, 100, 100))
      //g.drawString("Press left mouse button and drag to paint.", 100, size.height - 10)
      g.drawString("Press left mouse button and drag to paint.", 10, 10)
      g.setColor(Color.black)
      g.fillRect(0, size.height - 50, size.width, size.height)
      //g.draw(path)
    }
    add(but)
    add(but2)
    listenTo(this.mouse.clicks, this.mouse.moves, but)
    reactions += {
      case MouseDragged(src, point, mods) ⇒ println("mouse dragged: " + point)
      case MousePressed(src, point, i1, i2, b) ⇒ println("mouse pressed: " + point)
      case ButtonClicked(`but`) ⇒ println("clicked!!!")
      case e ⇒ //println("=> "+e.toString)
    }
  }

  class TestPanel extends MigPanel("wrap 2, w 100:500:1000") {
    //MigPanel.addSeparatorTo(this, "Dropped File")
    addHorizontalLabelSeparator("Dropped File")
    addHorizontalSeparator()

    add(new Label("Dir-Path"))
    val txtDirPath = new TextArea {
      columns = 20
      rows = 3
      editable = false
      lineWrap = true
      charWrap = true
    }
    add(txtDirPath, "grow, shrink")

    add(new Label("Dir-Name"))
    val txtDirName = new TextField {
      columns = 20
      editable = true
    }
    add(txtDirName, "grow")

    txtDirPath.border = txtDirName.border
    txtDirPath.background = txtDirName.background

    add(new Label("File-Name"))
    val txtFileName = new TextField {
      columns = 20
      editable = false
    }
    add(txtFileName, "grow")


    // add(new Separator(Orientation.Horizontal))
    //add(new MyBorderPanel(), "span, growx, growy 1000")
    add(new MyBorderPanel(), "span, h 30:300:1000, alignx center")
    //add(new MyBorderPanel(), "dock south")

    //border=new LineBorder(new Color(0,0,0), 1)
  }



}

