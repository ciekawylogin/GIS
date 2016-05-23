package gis

import java.io.File
import scala.sys.process._
import graph._

object EpidemySimulator {
  def writeToFile(p: String, s: String) = {
    val pw = new java.io.PrintWriter(new File(p))
    try pw.write(s) finally pw.close()
  }

  def main(args: Array[String]) = {
    val initialGraph = ScalefreeGraphGenerator.generateScalefreeGraph(50, 2, 4).withRandomVertexInfected
//    println(graph.V.toList.groupBy(_._2.degree()).map((v) => {
//      val (degree, list) = v
//      degree -> list.size
//    }).toList.sortBy(-_._1))
    val iterations = List.range(0, 50)
    val beta = 0.15
    val gamma = 0.1
    val results = iterations.foldLeft(initialGraph)((graph: Graph, iteration: Int) => {
      val iterationNum = "%05d".format(iteration)
      writeToFile(s"results/step$iterationNum.dot", graph.exportDot)
      s"dot results/step$iterationNum.dot -Tpng -O".!
      val newGraph = graph.simulationStep(beta, gamma)
      newGraph
    })
  }
}
