package gui

import scala.swing.{ Panel, Component, LayoutContainer, Orientation }
import net.miginfocom.swing.MigLayout
import javax.swing.JSeparator
import javax.swing.SwingConstants

object MigPanel {

  var SeparatorColor = new scala.swing.Color(0, 70, 213)

  def addSeparatorTo(panel: MigPanel, label: String): MigPanel = {
    import scala.swing.{ Label, Separator }

    val lbl = new Label(label)
    lbl.foreground = SeparatorColor

    val sep = new Separator //(Orientation.Vertical)

    panel.add(lbl, "gapbottom 1, span, split 2, aligny center")
    panel.add(sep, "gapleft rel, growx")
    panel
  }

  
}

class MigPanel(
    val layoutConstraints: String = "",
    val columnConstraints: String = "",
    val rowConstraints: String = "") extends Panel with LayoutContainer {

    var SeparatorColor = new scala.swing.Color(0, 70, 213)


   
    def addVerticalSeparator() = {
      import scala.swing.{ Separator }
      add(new Separator(Orientation.Vertical), "growy")
    }

    
    def addHorizontalSeparator() = {
      import scala.swing.{ Label, Separator }

      val sep = new Separator //(Orientation.Vertical)

      add(sep, "span, growx") //, gapleft rel, growx")
    }

    def addHorizontalLabelSeparator(label: String) = {
    import scala.swing.{ Label, Separator }

    val lbl = new Label(label)
    lbl.foreground = SeparatorColor

    val sep = new Separator

    add(lbl, "gapbottom 1, span, split 2, aligny bottom")
    //add(sep, "gapleft rel, growx, aligny bottom")
    add(sep, "growx, aligny bottom")
    //panel
  }



  override lazy val peer = {
    val mig = new MigLayout(
      layoutConstraints,
      columnConstraints,
      rowConstraints)
    new javax.swing.JPanel(mig) with SuperMixin
  }

  type Constraints = String

  private def layoutManager = peer.getLayout.asInstanceOf[MigLayout]

  protected def constraintsFor(comp: Component): Constraints =
    layoutManager.getComponentConstraints(comp.peer).asInstanceOf[String]

  protected def areValid(constr: Constraints): (Boolean, String) = (true, "")

  def add(comp: Component, constr: String): Unit = peer.add(comp.peer, constr)

  def add(comp: Component): Unit = add(comp, "")
}

