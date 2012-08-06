package gui
import swing._
import event._
import planning._
import structures._
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

  val probField = new TextField("Input Problem Name Here")
  
  val probBar = new FlowPanel {
    val browseBtn = new Button("Browse")
    probField.size_=(new Dimension(500, 100))
    probField.preferredSize_=(new Dimension(500, 27))
    contents += (probField, browseBtn)
    listenTo(probField, browseBtn)
    probField.selectAll()
    reactions +=
      {
        case FocusGained(text: TextField, _, _) =>
          probField.selectAll()
      }
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
        contents += (probBar, buttonbar, status)
        //        contents += buttonbar
        //        contents += status
      }) = BorderPanel.Position.South
    }

    size = new Dimension(600, 700)
  }

  def planAndShow() {
    graph.clear()
    val probName = probField.text
    val dir = "./planfiles/"
    val probFile = dir + probName + ".prob"
    val actionFile = dir + probName + ".act"
    val (p, stats) = planning.Main.pocl(actionFile, probFile)
    var statusText = stats.toString()
    if (p.isDefined) {
      graph.showPlan(p.get)
      statusText += ". Plan Found: " + p.get.toString
    } else statusText += ". No Plans Found."

    status.text_=(statusText)
  }

}