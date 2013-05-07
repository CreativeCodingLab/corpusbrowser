package gui

import scala.collection.mutable._
import swing._ //{ MainPanel, Panel, Component, LayoutContainer, Orientation }
import swing.event._
import javax.swing.border._
import javax.swing.border.Border._
import scala.Enumeration
import java.awt.{ Dimension, Color, Graphics, Insets }
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

object Main extends SimpleSwingApplication {

  Driver.loadBNCTest
   var viewTab1 = new ViewTab(1) 
 
  val desktopPane = new MyIDP
  val sentencePanel = new SentencePanel
  viewTab1.sentencePanels += sentencePanel
  desktopPane.add("sentence", sentencePanel)

  val concordancePanel = new ConcordancePanel
  viewTab1.patternPanels += concordancePanel
  desktopPane.add("concordance", concordancePanel)

  val colocationPanel = new ColocationPanel
  viewTab1.patternPanels += colocationPanel
  desktopPane.add("colocation", colocationPanel)

  val dispersionPanel = new DispersionPanel
    viewTab1.patternPanels += dispersionPanel
    desktopPane.add("dispersion", dispersionPanel)

    //dispersionPanel.patterns += new WordPattern("answer")
    //dispersionPanel.patterns += new CleanSentenceRegexPattern("""(?i)aware of.*not""".r)
    //concordancePanel.patterns += new CleanSentencePattern("""hat are the""".r)
    //concordancePanel.patterns += new CleanSentencePattern("""here""".r)
    //colocationPanel.patterns += new CleanSentencePattern("""t\w*e f\w*\b""".r)
    //colocationPanel.patterns += new CleanSentencePattern("""\bor the\b""".r)
    concordancePanel.patterns += new CleanSentencePattern("""\bhe\b""".r)
    colocationPanel.patterns += new CleanSentencePattern("""\bhe\b""".r)
    colocationPanel.patterns += new CleanSentencePattern("""\bshe\b""".r)
    dispersionPanel.patterns += new CleanSentencePattern("""\bhe\b""".r)
    dispersionPanel.patterns += new CleanSentencePattern("""\bshe\b""".r)
    //dispersionPanel.patterns += new CleanSentencePattern("""hat are the""".r)
    //dispersionPanel.patterns += new WordRegexPattern("""^t.*t$""")

    //dispersionPanel.patterns += new CleanTokenPattern("""a\w*y""")
    //dispersionPanel.patterns += new CleanTokenPattern("""any""")
    //dispersionPanel.patterns += new PartOfSpeechTokenPattern("""NP0...1""")
    //dispersionPanel.patterns += new RawSentencePattern("lemma=\"why\"")
    //dispersionPanel.patterns += new PartOfSpeechSentencePattern("DT0 AT0 NN2 PNP VVZ PR")
    //dispersionPanel.patterns += new PartOfSpeechSentencePattern("AT0 NN1")

    viewTab1.component = desktopPane
  

  var viewTab2 = new ViewTab(2) 
    val sentencePanel2 = new SentencePanel
    viewTab2.sentencePanels += sentencePanel2
    viewTab2.component = sentencePanel2 
  

  var currentView = viewTab1


    val navigatePanel = new NavigatePanel





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

  def switchView(vt:ViewTab) {
    currentView = vt
    navigatePanel.updateWidgets(true, true)
    val prevDividerLocation = topBottomPane.dividerLocation
    topBottomPane.topComponent = vt.component
    topBottomPane.dividerLocation=prevDividerLocation
    vt.component.requestFocus

  }

  def handleKeyPress(key: Value, modifiers: Int) {
    println("in Main.handleKeyPress : you pressed " + key)

    key match {
      case Key.Escape ⇒ {Main.exitProgram;}
      case Key.Key1 => switchView(viewTab1) 
      case Key.Key2 => switchView(viewTab2) 
      case Key.I => desktopPane.fireIconify
      case Key.K => desktopPane.fireDeiconify
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
    def this() = this(Orientation.Horizontal, currentView.component, navigatePanel);
    //def this() = this(Orientation.Horizontal, desktopPane, navigatePanel);
    //def this() = this(Orientation.Horizontal, tabbedPane, navigatePanel);

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

  class MyBorder(title:String) extends AbstractBorder {

    val TITLE_FONT = new java.awt.Font("Consolas", java.awt.Font.BOLD, 14)

    override def getBorderInsets(c:java.awt.Component) : java.awt.Insets = {
      new java.awt.Insets(20,1,1,1); //top, left, bottom, right
    }
   
    override def paintBorder(c:java.awt.Component, g:java.awt.Graphics, x:Int, y:Int, w:Int, h:Int) {
      val i = getBorderInsets(c);
       //g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
       g.setFont(TITLE_FONT)

      g.setColor(new Color(128,128,128));
      g.fillRect(0, 0, w, i.top);
      g.setColor(new Color(0,0,0));
      g.drawRect(0,0,w,i.top - 1)
      g.drawString(title, 50, 16)
      g.setColor(new Color(150,150,150))
      g.fillRect(28,0,13,i.top)
      g.setColor(new Color(0,0,0))
      g.drawRect(28,0,13,i.top-1)
      //g.drawRect(28,3,13,i.top-7)

      g.setColor(new Color(0,0,0))
      g.drawRect(0,0,w-1,h-1)
      
    }

    override def isBorderOpaque : Boolean = { true; }
  }

  class MyIF(title:String, x:Int, y:Int, pw:Int, ph:Int, mw:Int, mh:Int, thepanel:Panel) extends InternalFrame {
    implicit def tuple2Point(tuple: Tuple2[Int, Int]) = new Point(tuple._1, tuple._2)

    focusable
    //peer.putClientProperty("JInternalFrame.isPalette", java.lang.Boolean.TRUE);

    super.title = title
    bounds = (x,y,pw,ph)

    contents = thepanel;
    //thepanel.border = Swing.LineBorder(Color.BLACK, 1)
    border = new MyBorder(title) //Swing.LineBorder(Color.BLACK, 1)

    minimumSize = (mw, mh)
    preferredSize = (pw, ph)

    iconifiable = true
    closable = false //true
    maximizable = false //true
    resizable = true
    visible = true

    def fireIconify {
      println("iconifiying??????")
      //desktopPane.desktopManager.managerPeer.iconifyFrame(this.peer)
      icon = true
    }
    def fireDeiconify {
      println("deiconifiying??????")
      //desktopPane.desktopManager.deiconifyFrame(this.peer)
      icon = false
    }

    def handleKeyPress(key: Value, modifiers: Int) {
      println("in MyIF.. you pressed " + key)

      key match {
        case Key.F => {
          println("in MyIF... F")
          //this.peer.setBounds(100,100,100,600);
          revalidate
          repaint
        }
        case Key.V => {println("in MyIF... V")
        //this.setBounds(400,100,300,600);
        revalidate
        repaint
      }
      case _ => Main.handleKeyPress(key, modifiers) 
    }
  }

 
  /*
  listenTo(this.frame, this.keys)
  reactions += {
    case KeyPressed(src, key, modifiers, location) ⇒ {println("in MyIF: key = " + key); handleKeyPress(key, modifiers)}
    case InternalFrameActivated(src, _) => {
      println("InternalFrame Activated: " + src.title)
      desktopPane.selectedFrame = this
      new javax.swing.event.InternalFrameEvent(src.peer, javax.swing.event.InternalFrameEvent.INTERNAL_FRAME_ACTIVATED )
    }
    case InternalFrameIconified(src, _) => {
      println("Inconified... " + src)
      new javax.swing.event.InternalFrameEvent(src.peer, javax.swing.event.InternalFrameEvent.INTERNAL_FRAME_ICONIFIED )
    }
    case InternalFrameDeiconified(src, _) => {
      println("Deinconified... " + src)
      new javax.swing.event.InternalFrameEvent(src.peer, javax.swing.event.InternalFrameEvent.INTERNAL_FRAME_DEICONIFIED )

    }
    case InternalFrameOpened(src, _) =>  println("opened... " + src)
    case InternalFrameDeactivated(src, _) =>  println("Deactivated... " + src)
    case _ => println
  }
  */

  /*
  listenTo(this.frame, this.keys)
  reactions += {
    case KeyPressed(src, key, modifiers, location) ⇒ handleKeyPress(key, modifiers)
    case InternalFrameActivated(src, _) => {
      println("InternalFrame Activated: " + src.title)

      val r:java.awt.Rectangle = peer.getBounds
      println(r.x + "/" + r.y + "/" + r.width + "/" + r.height)
      //bounds = (10,10,300,300)

      toggleControlPanel(true)

      src.minimumSize = (250, 300)
      src.preferredSize = (250, 300)

      src.revalidate
      src.repaint
      thepanel.minimumSize = (250, 100)
      thepanel.preferredSize = (250, 100)
      thepanel.revalidate
      thepanel.repaint

      desktopPane.revalidate
      desktopPane.repaint

    }


    case InternalFrameDeactivated(src, _) => {
      println("InternalFrame Deactivated: " + src.title)
      bounds = (10,30,100,100)

      toggleControlPanel(false)
      src.minimumSize = (150, 100)
      src.preferredSize = (150, 100)
      src.revalidate
      src.repaint
      thepanel.minimumSize = (150, 100)
      thepanel.preferredSize = (150, 100)
      thepanel.revalidate
      thepanel.repaint
      desktopPane.revalidate
      desktopPane.repaint
    }
    //case _ => println _
  }
  */

  def toggleControlPanel(isOn:Boolean) {
    thepanel match {
      case pp:PatternPanel => {
        println("this is a pattern panel!")
        if (isOn) pp.turnOnControlPanel else pp.turnOffControlPanel
      }
      case x:SentencePanel => {
        println("this is a sentence panel!")
      }
      //case _ => println _
    }
  }
}

class MyIDP() extends InternalDesktopPane {
  focusable
  listenTo(this.keys)
  reactions += {
    case KeyPressed(src, key, modifiers, location) ⇒ {println("in MyIDP, key = " + key); handleKeyPress(key, modifiers)}
    case _ =>
  } 

  var selectedFrame:MyIF = _
  
  dragMode = DragMode.Outline //Live //Outline
  //background= new Color(255,255,255)
  background= new Color(150,150,150)

  def add(title:String, dataPanel:DataPanel) {
    add(new MyIF(title, 100, 10, 300, 200, 250, 100, dataPanel))
  }
  
  def add(ifr: MyIF) { //InternalFrame) {
    //tmp
    selectedFrame = ifr
    super.add(ifr, new LayerConstraints)
  }

  
  def fireDeiconify {
    if (selectedFrame != null) {
      println("SF : " + selectedFrame.peer.isIcon + "/" + selectedFrame.peer.isSelected)
        println("DeICONIFY!")
        selectedFrame.fireDeiconify
    }
  }

  def fireIconify {
    if (selectedFrame != null) {
      println("SF : " + selectedFrame.peer.isIcon + "/" + selectedFrame.peer.isSelected)
      if (selectedFrame.peer.isIcon) {
        println("DeICONIFY!")
        selectedFrame.fireDeiconify
      } else {
        println("ICONIFY!")
        selectedFrame.fireIconify
      }
    }
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

  //add(new Separator(Orientation.Horizontal))
  //add(new MyBorderPanel(), "span, growx, growy 1000")
  add(new MyBorderPanel(), "span, h 30:300:1000, alignx center")
  //add(new MyBorderPanel(), "dock south")

  //border=new LineBorder(new Color(0,0,0), 1)
}







}

