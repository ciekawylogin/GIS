package gis.graph

import scala.util.Random

case class Graph(V: Map[String, Vertex]) {
  def withVertex(v: (String, Vertex)) = {
    val (newName, newVertex) = v
    val editedVertices = newVertex.neighbors.map(
      (name: String) => name -> getVertex(name).withNeighbor(newName)
    )
    this.copy(V ++ editedVertices.toMap + v)
  }

  def withRandomVertexInfected = {
    val vertexToInfect = randomVertexName
    copy(V = V + (vertexToInfect -> getVertex(vertexToInfect).infected))
  }

  def getVertex(name: String) =
    V.get(name).get

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
      val vertex = getVertex(name)
      if(acc <= num && acc + vertex.degree > num)
        result = Some(name)
      acc + vertex.degree
    })
    result.get
  }

  def simulationStep(beta: Double, gamma: Double) = {
    copy(
      V = V.map({
      case (name: String, vertex: Vertex) if vertex.state == State.Infected =>
        if(gamma > Random.nextDouble())
          name -> vertex.susceptible
        else
          name -> vertex
      case (name: String, vertex: Vertex) if vertex.state == State.Susceptible =>
        val infectedNeighbors = vertex.neighbors.foldLeft(0)((acc: Int, neighborName: String) => {
          val neighbor = getVertex(neighborName)
          if (neighbor.state == State.Infected)
            acc + 1
          else
            acc
        })
        if(beta * infectedNeighbors > Random.nextDouble())
          name -> vertex.infected
        else
          name -> vertex
    }))
  }

  lazy val exportDot = {
    val header = "graph graphname {"
    val options = ""
    val footer = "}"
    val nodes = V.keys.fold("")((acc: String, name: String) => {
      val node = getVertex(name)
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
