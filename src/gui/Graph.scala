package gui
import swing._
import javax.swing._
import java.awt.Font
import java.awt.FontMetrics
import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.layout._
import com.mxgraph.util._
import com.mxgraph.view.mxGraph
import com.mxgraph.model._
import java.awt.Font
import planning._
import structures._

import scala.collection.mutable.HashMap
import action._

class Graph {

  protected var graph = new mxGraph()
  initGraph()
  protected val graphComp = new mxGraphComponent(graph)
  initComponent()
  //graphComp.set
  protected var layout = new mxCircleLayout(graph, 5)

  val component = Component.wrap(graphComp)
  graph.setAutoOrigin(true)
  graph.setAutoSizeCells(true)

  def insertVertex(id: Int, text: String) {
    val parent = graph.getDefaultParent()
    val v1 = graph.insertVertex(parent, id.toString, text, 0, 0, 10, 10)
    //    graph.

    layout.execute(graph.getDefaultParent())
  }

  def defaultFill() {
    defineStyles()
    val parent = graph.getDefaultParent()
    graph.getModel().beginUpdate()
    try {
      var x = 5; var y = 5
      val texts = List("Hello") //, "World!", "Life", "Is", "Awesome", "Thomas Edison")
      texts foreach { text =>
        insertCell(text, x, y)
        x += 50
        y += 50
      }
      //      val v1 = graph.insertVertex(parent, null, "Hello", 20, 20, 100,
      //        40)
      //      val v2 = graph.insertVertex(parent, null, "World!", 240, 150,
      //        80, 40, "Blue")
      //      val v3 = graph.insertVertex(parent, null, "Life", 240, 150,
      //        80, 40, "Yellow")
      //      val v4 = graph.insertVertex(parent, null, "is", 240, 150,
      //        80, 40, "Red")
      //      val v5 = graph.insertVertex(parent, null, "Awesome.", 240, 150,
      //        80, 40, "Blue")
      //      graph.insertEdge(parent, null, "Edge1", v1, v2)
      //      graph.insertEdge(parent, null, "Edge2", v1, v3)
      //      graph.insertEdge(parent, null, "Edge3", v2, v4)
      //      graph.insertEdge(parent, null, "Edge4", v1, v5)
      //      graph.insertEdge(parent, null, "Edge5", v4, v5)
      //      graph.insertEdge(parent, null, "Edge5", v5, v3)
      //
      //      val obj = new AnyRef {
      //        override def toString() = "new object here"
      //      }
      //      graph.addCell(obj)
    } finally {
      graph.getModel().endUpdate()
    }
    //layout.execute(graph.getDefaultParent())
  }

  protected def initGraph() {
    graph.setCellsEditable(false)
    graph.setCellsResizable(false)
    graph.setCellsDisconnectable(false)
    graph.setEdgeLabelsMovable(false)
    graph.setCellsDeletable(false)
    graph.setCellsMovable(true)
  }

  protected def initComponent() {
    graphComp.setConnectable(false);
    graphComp.getViewport().setOpaque(true);
    graphComp.getViewport().setBackground(java.awt.Color.white);
    defineStyles()
    layout = new mxCircleLayout(graph, 5)
  }

  def insertCell(text: String, x: Int, y: Int) {
    val parent = graph.getDefaultParent()
    val (xplus, yplus) = getsize(text)
    val cell = new mxCell(new AnyRef {
      override def toString() = "olha"
    }, new mxGeometry(x, y, xplus, yplus), "Blue")
    cell.setVertex(true)
    cell.setConnectable(true)
    graph.addCell(cell, parent)
  }

  def makeCell(text: String): mxCell = {
    val parent = graph.getDefaultParent()
    val (xplus, yplus) = getsize(text)
    val cell = new mxCell(text, new mxGeometry(0, 0, xplus, yplus), "Blue")
    cell.setVertex(true)
    cell.setConnectable(true)
    cell
  }

  def makeEdge(text: String): mxCell = {
    val edge = new mxCell(text, new mxGeometry(), "solidEdge");
    edge.setEdge(true);
    edge.getGeometry().setRelative(true)
    edge
  }

  def makeDashedEdge(text: String): mxCell = {
    val edge = new mxCell(text, new mxGeometry(), "dashedEdge");
    edge.setEdge(true);
    edge.getGeometry().setRelative(true)
    edge
  }

  def clear() {
    graph = new mxGraph
    initGraph()
    graphComp.setGraph(graph)
    initComponent()
  }

  def getsize(text: String): (Int, Int) =
    {
      val graphics = graphComp.getGraphics()
      val font = new Font("Arial", Font.PLAIN, 18)
      val metric = graphics.getFontMetrics(font)
      val width = metric.stringWidth(text) + 40
      val height = metric.getHeight() + 15
      (width, height)
    }

  def setSize(d: Dimension) {
    graphComp.setPreferredSize(d)
  }

  def defineStyles() {
    val style1 = new java.util.Hashtable[String, Object]()
    style1.put(mxConstants.STYLE_FILLCOLOR, "0x408DD2")
    style1.put(mxConstants.STYLE_STROKEWIDTH, 0.asInstanceOf[AnyRef])
    style1.put(mxConstants.STYLE_FONTSIZE, 18.asInstanceOf[AnyRef])
    style1.put(mxConstants.STYLE_GLASS, true.asInstanceOf[AnyRef])
    style1.put(mxConstants.STYLE_FONTCOLOR, "0xFFFFFF")
    style1.put(mxConstants.STYLE_SPACING_TOP, 3.asInstanceOf[AnyRef])
    style1.put(mxConstants.STYLE_GRADIENTCOLOR, "0x043A6B")

    val stylesheet = graph.getStylesheet();
    stylesheet.putCellStyle("Blue", style1);
    //stylesheet.setDefaultVertexStyle(style1)

    val style2 = new java.util.Hashtable[String, Object]()
    style2.put(mxConstants.STYLE_FILLCOLOR, "0xFFDD00")
    style2.put(mxConstants.STYLE_STROKEWIDTH, 0.asInstanceOf[AnyRef])
    style2.put(mxConstants.STYLE_FONTSIZE, 18.asInstanceOf[AnyRef])
    style2.put(mxConstants.STYLE_GLASS, true.asInstanceOf[AnyRef])
    style2.put(mxConstants.STYLE_FONTCOLOR, "0xFFFFFF")
    style2.put(mxConstants.STYLE_SPACING_TOP, 3.asInstanceOf[AnyRef])
    style2.put(mxConstants.STYLE_GRADIENTCOLOR, "0xA68F00")

    stylesheet.putCellStyle("Yellow", style2);

    val style3 = new java.util.Hashtable[String, Object]()
    style3.put(mxConstants.STYLE_FILLCOLOR, "0xFC3F4D")
    style3.put(mxConstants.STYLE_STROKEWIDTH, 0.asInstanceOf[AnyRef])
    style3.put(mxConstants.STYLE_FONTSIZE, 18.asInstanceOf[AnyRef])
    style3.put(mxConstants.STYLE_GLASS, true.asInstanceOf[AnyRef])
    style3.put(mxConstants.STYLE_FONTCOLOR, "0xFFFFFF")
    style3.put(mxConstants.STYLE_SPACING_TOP, 3.asInstanceOf[AnyRef])
    style3.put(mxConstants.STYLE_GRADIENTCOLOR, "0xA2000C")

    stylesheet.putCellStyle("Red", style3);

    val edgeStyle = new java.util.Hashtable[String, Object]()
    edgeStyle.put(mxConstants.STYLE_ROUNDED, true.asInstanceOf[AnyRef]);
    edgeStyle.put(mxConstants.STYLE_EDGE, mxConstants.EDGESTYLE_LOOP); // <-- This is what you want
    edgeStyle.put(mxConstants.STYLE_SHAPE, mxConstants.SHAPE_CONNECTOR);
    edgeStyle.put(mxConstants.STYLE_ENDARROW, mxConstants.ARROW_CLASSIC);
    edgeStyle.put(mxConstants.STYLE_VERTICAL_ALIGN, mxConstants.ALIGN_MIDDLE);
    edgeStyle.put(mxConstants.STYLE_ALIGN, mxConstants.ALIGN_CENTER);
    edgeStyle.put(mxConstants.STYLE_STROKECOLOR, "#6482B9");
    edgeStyle.put(mxConstants.STYLE_FONTCOLOR, "#000000");

    stylesheet.setDefaultEdgeStyle(edgeStyle)
    stylesheet.putCellStyle("solidEdge", edgeStyle);

    val dashedStyle = new java.util.Hashtable[String, Object]()
    dashedStyle.put(mxConstants.STYLE_ROUNDED, true.asInstanceOf[AnyRef]);
    dashedStyle.put(mxConstants.STYLE_EDGE, mxConstants.EDGESTYLE_LOOP); // <-- This is what you want
    dashedStyle.put(mxConstants.STYLE_SHAPE, mxConstants.SHAPE_CONNECTOR);
    dashedStyle.put(mxConstants.STYLE_ENDARROW, mxConstants.ARROW_CLASSIC);
    dashedStyle.put(mxConstants.STYLE_VERTICAL_ALIGN, mxConstants.ALIGN_MIDDLE);
    dashedStyle.put(mxConstants.STYLE_ALIGN, mxConstants.ALIGN_CENTER);
    dashedStyle.put(mxConstants.STYLE_STROKECOLOR, "#6482B9");
    dashedStyle.put(mxConstants.STYLE_FONTCOLOR, "#000000");
    dashedStyle.put(mxConstants.STYLE_DASHED, true.asInstanceOf[AnyRef])

    stylesheet.putCellStyle("dashedEdge", dashedStyle);
  }

  /**
   * returns a list of vertices (1st) and a list of edges (2nd)
   *
   */
  def showPlan(plan: Plan): (Array[mxCell], List[mxCell]) =
    {
      val parent = graph.getDefaultParent()
      val count = plan.stepCount + 2
      val cellArr = new Array[mxCell](count)

      plan.steps foreach {
        step =>
          val name = {
            if (step.id == Constants.INIT_ID) "Init"
            else if (step.id == Constants.GOAL_ID) "Goal"
            else step.id + ". " + plan.binding.substVarsString(step).replace("'", "")
          }
          if (step.id == Constants.GOAL_ID)
            cellArr(count - 1) = makeCell(name)
          else cellArr(step.id) = makeCell(name)
      }

      cellArr.foreach { graph.addCell(_, parent) }

      var hash = new HashMap[(Int, Int), String]()
      plan.links foreach { link =>
        val pair = (link.id1, link.id2)
        val value = hash.get(pair)
        val condition = plan.binding.substVars(link.precondition).toString
        val s =
          if (value.isDefined) value.get + ", " + condition
          else condition
        hash.put(pair, s)
      }

      var edges = hash.keySet map { key =>
        val name = key._1 + "->" + {
          if (key._2 == Constants.GOAL_ID) "goal" else key._2
        } + " " + hash.get(key).get
        val edge = makeEdge(name.replace("'", ""))
        val source = cellArr(key._1)
        val target = if (key._2 == Constants.GOAL_ID) cellArr(count - 1) else cellArr(key._2)
        graph.addEdge(edge, parent, source, target, null)
        edge
      }
      val ordering = plan.ordering.necessary()
      var temporals = ordering filterNot { hash.keySet contains (_) }
      //println("ordering: " + ordering.mkString("   "))
      //println("keyset: " + hash.keySet.mkString("   "))
      //println("temporal: " + temporals.mkString("   "))
      val tempedges = temporals map {
        pair =>
          val name = pair._1 + "->" + {
            if (pair._2 == Constants.GOAL_ID) "goal" else pair._2
          }
          val edge = makeEdge(name)
          val source = cellArr(pair._1)
          val target = if (pair._2 == Constants.GOAL_ID) cellArr(count - 1) else cellArr(pair._2)
          graph.addEdge(edge, parent, source, target, null)
          edge
      }

      edges = edges ++ tempedges

      layout.execute(graph.getDefaultParent())
      return (cellArr, edges.toList)
    }
}