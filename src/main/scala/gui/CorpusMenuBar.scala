package gui

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
import java.io._


class CorpusMenuBar extends MenuBar {
  contents += new Menu("A Menu") {         
    contents += new MenuItem("An item")        
    contents += new MenuItem(Action("Action item") { println("aaa") }) 
    contents += new Separator  
    contents += new CheckMenuItem("Check me")       
  } 
  contents += new Menu("Empty Menu") 
}   
