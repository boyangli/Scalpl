package gui
import swing._
import event._
import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.view.mxGraph

object BasicSwing extends SimpleSwingApplication {

  val centerDim = new Dimension(600, 500)
  val bottomDim = new Dimension(600, 100)
  val graph = new Graph()

  val textarea = new TextArea {
    size = centerDim
  }
  // status bar
  val status = new TextField {
    editable = false
    size = bottomDim
  }

  val planBtn = new Button("Plan")
  val btn2 = new Button("button2")
  // button bar
  val buttonbar = new FlowPanel {
    contents += (planBtn, btn2)
    listenTo(planBtn, btn2)
    reactions +=
      {
        case ButtonClicked(button) =>
          if (button == btn2)
            graph.defaultFill()
          else if (button == planBtn) {
            planAndShow()
          }
      }

  }

  graph.setSize(centerDim)

  def top = new MainFrame {
    title = "Scalpo Partial-Order Planner"
    contents = new BorderPanel {
      layout(graph.component) = BorderPanel.Position.Center

      layout(new BoxPanel(Orientation.Vertical) {
        contents += status
        contents += buttonbar
      }) = BorderPanel.Position.South
    }

    size = new Dimension(600, 700)

  }

  def planAndShow() {
    val p = plan.Main.plan()

    println(p)
    if (p.isDefined)
      graph.showPlan(p.get)
  }

}