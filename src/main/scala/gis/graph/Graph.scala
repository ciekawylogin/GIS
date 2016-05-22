package gis.graph

import scala.util.Random

case class Graph(V: Map[String, Vertex]) {
  def withVertex(v: (String, Vertex)) = {
    val (newName, newVertex) = v
    val editedVertices = newVertex.neighbors.map(
      (name: String) => name -> V.get(name).get.withNeighbor(newName)
    )
    this.copy(V ++ editedVertices.toMap + v)
  }

  def withRandomVertexInfected = {
    val vertexToInfect = randomVertexName
    copy(V = V + (vertexToInfect -> V.get(vertexToInfect).get.infected))
  }

  lazy val totalDegree =
    V.values.map((v: Vertex) => v.degree()).sum

  lazy val isEmpty =
    V.isEmpty

  def randomVertexName =
    Random.shuffle(V).head._1

  def weightedRandomVertexName = {
    val num: Int = math.floor(Random.nextDouble() * totalDegree).toInt
    var result: Option[String] = None
    V.keys.foldLeft(0)((acc: Int, name: String) => {
      val vertex = V.get(name).get
      if(acc <= num && acc + vertex.degree > num)
        result = Some(name)
      acc + vertex.degree
    })
    result.get
  }

  lazy val exportDot = {
    val header = "graph graphname {"
    val options = ""
    val footer = "}"
    val nodes = V.keys.fold("")((acc: String, name: String) => {
      val node = V.get(name).get
      acc + node.neighbors.map((neighborName: String) => {
        if (neighborName > name)
          s""" "${name.replace(" ", "\\n")}" -- "${neighborName.replace(" ", "\\n")}"\n"""
        else
          ""
      }).fold("")(_ + _)
    })
    s"$header\n$options\n$nodes\n$footer"
  }
}
