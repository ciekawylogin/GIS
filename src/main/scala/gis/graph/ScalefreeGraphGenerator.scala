package gis.graph

import gis.NameGenerator

object ScalefreeGraphGenerator {
  def generateClique(size: Int) =
    List.range(0, size).foldLeft(new Graph(Map()))((graph: Graph, _: Int) => graph.withVertex(
      NameGenerator.generateName() -> Vertex(graph.V.keys.toSeq)
    ))

  def generateScalefreeGraph(size: Int, initialSize: Int, m: Int = 1) =
    List.range(initialSize, size).foldLeft(generateClique(initialSize))((graph: Graph, _: Int) => graph.withVertex(
      NameGenerator.generateName() -> Vertex(
//        graph.V.flatMap(v => {
//          val (name, vertex) = v
//          if(vertex.degree() > math.random * graph.totalDegree)
//            Some(name)
//          else
//            None
//        }).toList
        List.range(0, m).map(_ => graph.weightedRandomVertexName)
      )
    ))
}
