package gis

import graph._

object EpidemySimulator {
  def main(args: Array[String]) = {
    val graph = ScalefreeGraphGenerator.generateScalefreeGraph(50, 3)
//    println(graph.V.toList.groupBy(_._2.degree()).map((v) => {
//      val (degree, list) = v
//      degree -> list.size
//    }).toList.sortBy(-_._1))
    println(graph.exportDot)
  }
}
