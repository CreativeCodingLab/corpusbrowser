package gui


import org.interactivemesh.scala.swing._
import org.interactivemesh.scala.swing.event._
import java.io._
import swing._ //{ MainPanel, Panel, Component, LayoutContainer, Orientation }
import swing.event._
import javax.swing.border._
import javax.swing.border.Border._
import scala.Enumeration
import java.awt.Font
import java.awt.FontMetrics
import java.awt.RenderingHints

import java.awt.{ Dimension, Color }
import javax.swing.JSeparator
import javax.swing.{JInternalFrame, JDesktopPane}
import javax.swing.SwingConstants
import scala.swing.event.Key._
import CorpusData._
//import scala.xml._
import scala.swing._
import Swing._
import scala.swing.event._
import Tree._
import java.awt.Color
import corpus2.Driver._
import corpus2._

/*
 val objectGraphTree = new Tree[Any] {
        treeData = TreeModel[Any](orders: _*)({
          case o @ Order(_, cust, prod, qty) => Seq(cust, prod, "Qty" -> qty, "Cost" -> ("$" + o.price))
          case Product(id, name, price) => Seq("ID" -> id, "Name" -> name, "Price" -> ("$" + price))
          case Customer(id, _, first, last) => Seq("ID" -> id, "First name" -> first, "Last name" -> last)
          case _ => Seq.empty
        })

        renderer = Renderer({
          case Order(id, _, _, 1) => "Order #" + id
          case Order(id, _, _, qty) => "Order #" + id + " x " + qty
          case Product(id, _, _) => "Product " + id
          case Customer(_, title, first, last) => title + " " + first + " " + last
          case (field, value) => field + ": " + value
          case x => x.toString
        })
        
        listenTo(selection)
        reactions += {
          case TreeNodeSelected(node) => println("Selected: " + node)
        }
        
        expandAll()
      }
      
      //documentId: Int, code: String, title: String, sentences: ListBuffer[Int]
  */

 // trait CorpusTree { this: Tree[Any] =>
 //    treeData = TreeModel[Any](corpora: _*)({
 //        case c @ Corpus(corpusId, corpusRootDir, corpusType) => c.documents.map{i => documents(i)}
 //        case d @ Document(documentId, code, title, sentences) => d.sentences 
 //        case _ => Seq.empty
 //      })
 //    
    

 
 trait CorpusTree { this: Tree[CorpusData] =>
    //treeData = TreeModel[CorpusData](Main.rootNode: _*)({
    treeData = TreeModel[CorpusData](Main.rootNode)({
        case c @ CorpusData(name) => c.children
        case _ => Seq.empty
      })
    
     
    

    
    // treeData = TreeModel[Any](documents: _*)({
    //     case _ => Seq.empty
    //   })


    //treeData = TreeModel[Any](documents: _*)({
      //case Document(documentId, code, title, sentences) => sentences
      //})
  
    //renderer = new Tree.LabelRenderer({f => 
      //val iconFile = "/scala/swing/example/" + (if (f.isDirectory) "folder.png" else "file.png")
      //val iconURL = Main.resourceFromClassloader(iconFile) ensuring (_ != null, "Couldn't find icon " + iconFile) 
      //(Icon(iconURL), f.getName)
   // })
  // 
  //   renderer = new DefaultRenderer {
  //     backgroundSelectionColor = new Color(255,255,255);
  //     textSelectionColor(Color.BLACK)
  //     borderSelectionColor = new Color(0,0,0)
  //   }

  var TREE_FONT = new Font("Menlo", Font.PLAIN, 12)

    class MyRenderer[-A] extends DefaultRenderer[A] {

      
      override def paintComponent(g2: Graphics2D) {
         g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

         g2.setFont(TREE_FONT)
         super.paintComponent(g2)


      }

      override def componentFor(tree: Tree[_], value: A, info: Renderer.CellInfo): Component = {
     
        peer.setPreferredSize(70,100)

       //print(info)
        value match {
          case c:CorpusData => {
            //val nodeColor = c.getColor
            if (c.isSelected == true) {
         
              backgroundNonSelectionColor = c.color
              backgroundSelectionColor = c.color
            } else {
              backgroundNonSelectionColor = Color.WHITE
              backgroundSelectionColor = Color.WHITE
            }

            //temp until we put in real colors...
            //backgroundNonSelectionColor = if (c.isLeaf == true) new Color(0,255,0) else new Color(255,0,0)
            //backgroundSelectionColor = if (c.isLeaf == true) new Color(0,255,0) else new Color(255,0,0)
          }
          case _ => //it shouldn't be anything else
        }    
          
        //backgroundSelectionColor = new Color(255,255,0,100)
        textSelectionColor(Color.BLACK)
        borderSelectionColor = null //new Color(0,0,0)

        peer.defaultRendererComponent(tree.peer, value.asInstanceOf[AnyRef], info.isSelected, info.isExpanded, info.isLeaf, info.row, info.hasFocus)
        this
      }
      
    }

    renderer = new MyRenderer
  }
  
  class TreePanel extends Tree[CorpusData] with CorpusTree {
    //expandRow(0)
    //expandAll()

     border=new LineBorder(new Color(255,255,255), 10)

     
     listenTo(this.mouse.clicks, this.keys) //, this.selection)
      reactions += {
        case KeyPressed(src, key, modifiers, location) ⇒ Main.handleKeyPress(key, modifiers)
        case MousePressed(src, point, i1, i2, b) ⇒  handleMousePressed(point,i2)
        // case TreeNodeSelected(node) => node match {
        //   case c:Corpus => handleCorpusSelection(c) 
        //   case d:Document => handleDocumentSelection(d) 
        //   case _ => println("huh?")
        // }
      }

      def handleMousePressed(e: Point, clickCount: Int) {
        
        val selRow = peer.getRowForLocation(e.getX.toInt, e.getY.toInt);
        val selPath = peer.getPathForLocation(e.getX.toInt, e.getY.toInt);

        if(selRow != -1)
        {
          println("objects : " + selPath.getPathCount());
          println("last path component = " + selPath.getLastPathComponent());

          val node = selPath.getLastPathComponent()

          node match {
            case c:CorpusData => handleNodeSelection(c) 
            //case c:Corpus => handleCorpusSelection(c) 
            //case d:Document => handleDocumentSelection(d) 
            case _ => println("huh?")
          }
        }

        /*
        private void mySingleClick(TreePath path)
  {
    // int rc1 = (int) (Math.random() * 255);
    //         int rc2 = (int) (Math.random() * 255);
    //         int rc3 = (int) (Math.random() * 255);
    //         renderer.setTextSelectionColor(new Color(rc1, rc2, rc3));
    //System.out.println("single row/path = " + row + "/" + path);
    System.out.println("objects : " + path.getPathCount());
    System.out.println("last path component = " + path.getLastPathComponent());
    DefaultMutableTreeNode node = (DefaultMutableTreeNode) path.getLastPathComponent();
    
    
    CorpusData cd = (CorpusData) node.getUserObject();
    
      cd.toggle();
      recurseChildToSetSelection(node, cd.getIsSelected());
      Main.navigatePanel.initializeRangeSettings();
  }
      */     
      }
      
      def handleNodeSelection(node: CorpusData) {
        println("you selected a CorpusData node:" + node)
        CorpusData.toggleNode(node)
        repaint
      }
      def handleDocumentSelection(doc: Document) {
        println("you selected Document:" + doc)
        //CorpusData.toggleDocument(doc)
      }

      def handleCorpusSelection(corpus: Corpus) {
        println("you selected Corpus:" + corpus)     
      }

      
    }


