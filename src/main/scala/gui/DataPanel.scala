package gui

import swing._ //{ MainPanel, Panel, Component, LayoutContainer, Orientation }
import swing.BorderPanel._
import swing.ScrollPane._ //BarPolicy._
import java.awt.Font
import java.awt.FontMetrics
import java.awt.RenderingHints
import scala.swing.event._
import scala.swing.event.Key._
import javax.swing.ScrollPaneConstants
import javax.swing.SwingConstants
import javax.swing.ScrollPaneConstants._
import CorpusData._
import corpus2._
import corpus2.Driver._
import patterns._
import java.awt.Color


abstract class DataPanel() extends BorderPanel {
  implicit def tuple2Dimension(tuple: Tuple2[Int, Int]) = new Dimension(tuple._1, tuple._2)

  focusable = true

  //itensive, time consuming panels probablywant this set to false, i.e. concordance
  var updateAlways:Boolean
  
  //the main 2 panels and the scrollPane for the display
  val display:DisplayPanel
  val control:ControlPanel
  val jsp:DisplayScrollPane 

  listenTo(this, this.keys)
  reactions += {
    case scala.swing.event.ComponentResized(src) => {
      handleResized(src)
    }
    case KeyPressed(src, key, modifiers, location) ⇒ {println("in dataPanel.container, key = " + key); handleKeyPress(key, modifiers)}
    
    //case FocusGained(src, _, _) => println("conc: you gained focuse!!!")
    //case FocusLost(src, _, _) => println("conc: you lost focuse!!!")
    case _ =>
  }

 
  def turnOnControlPanel {
    control.preferredSize = (control.size.width, 60)
    control.minimumSize = (control.size.width, 60)
    control.maximumSize = (control.size.width, 60)
    revalidateAll
  }

  def turnOffControlPanel {
    control.preferredSize = (control.size.width, 0)
    control.minimumSize = (control.size.width, 0)
    control.maximumSize = (control.size.width, 0)
    revalidateAll
  }

  def handleResized(src:Component) {}

   def handleKeyPress(key: Value, modifiers: Int) {
      key match {
        case Key.A => turnOnControlPanel
        case Key.Z => turnOffControlPanel
        case _ => Main.handleKeyPress(key, modifiers) 
      }
    }

  abstract class DisplayPanel extends Component {
   focusable = true
     listenTo(this, this.mouse.clicks, this.mouse.moves, this.keys)
     reactions += {
      case MouseMoved(src, point, mods) ⇒ {handleMoved(point, mods)}
      case MousePressed(src, point, i1, i2, b) ⇒  handlePressed(point, i1, i2) 
      case KeyPressed(src, key, modifiers, location) ⇒ {println("in dataPanel.display, key = " + key); handleKeyPress(key, modifiers)}
      // case FocusGained(src, _, _) => println("display: you gained focuse!!!")
      // case FocusLost(src, _, _) => println("display: you lost focuse!!!")
      case _ => //println("in abstract reactions... ")
    }

    def handlePressed(me: Point, modifiers: Int, clickCount: Int) {
     requestFocus
    }
    
    def handleMoved(me: Point, mods: Int) {}
    
  

    def clear(g2: Graphics2D) {
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      g2.setColor(Color.WHITE);
      g2.fillRect(0,0, size.width, size.height);
    }



  }

  abstract class ControlPanel(miglayout:String) extends MigPanel(miglayout) { }
  
  //class DisplayScrollPane(c:Component, hp:Value, vp:Value) extends ScrollPane(c) {
  class DisplayScrollPane(hp:BarPolicy.Value, vp:BarPolicy.Value) extends ScrollPane(display) {
    border=null
    horizontalScrollBarPolicy = hp //BarPolicy.Never //AsNeeded
    verticalScrollBarPolicy = vp //BarPolicy.AsNeeded
    peer.setVerticalScrollBar(new DisplayScrollBar(1))
    peer.setHorizontalScrollBar(new DisplayScrollBar(0))
  }
 
  class DisplayScrollBar(orientation:Int) extends javax.swing.JScrollBar(orientation) {
    setSize(0,0)
    setMaximumSize(new java.awt.Dimension(0,0))
    setPreferredSize(new java.awt.Dimension(0,0))

    override def paintComponent(g: java.awt.Graphics) {
      //g.setColor(new Color(128,0,0));
      //g.fillRect(0, 0, 10, 10);
    }

  }

  def revalidateAll {
    display.revalidate
    display.repaint
    control.revalidate
    control.repaint
    jsp.revalidate
    jsp.repaint
  }

}
