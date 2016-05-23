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
    V.values.map((v: Vertex) => v.degree).sum

  lazy val isEmpty =
    V.isEmpty

  def randomVertexName =
    Random.shuffle(V).head._1

  def weightedRandomVertexName = {
    val num: Int = math.floor(Random.nextDouble() * totalDegree).toInt
    var result: Option[String] = None
    V.foldLeft(0)({
      case (acc: Int, (name: String, vertex: Vertex)) =>
        if(acc <= num && acc + vertex.degree > num)
          result = Some(name)
        acc + vertex.degree
    })
    result.get
  }

  def simulationStep(beta: Double, gamma: Double) = {
    copy(
      V = V.map({
      case (name: String, vertex: Vertex) if vertex.isInfected =>
        if(gamma > Random.nextDouble())
          name -> vertex.susceptible
        else
          name -> vertex
      case (name: String, vertex: Vertex) if vertex.isSusceptible =>
        val infectedNeighbors = vertex.neighbors.foldLeft(0)((acc: Int, neighborName: String) => {
          val neighbor = getVertex(neighborName)
          if (neighbor.isInfected)
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

  def countIntectedVertices =
    V.count {
      case (_: String, v: Vertex) => v.isInfected
    }

  lazy val exportDot = {
    val header = "graph graphname {"
    val options = """
                    rankdir=LR
                    ratio=1.5
                    nodesep=0.1
                    ranksep=0.5
                  """
    val footer = "}"
    val nodes = V.foldLeft("")({
      case (acc: String, (name: String, node: Vertex)) if node.isSusceptible =>
        acc + s""" "${name.replace(" ", "\\n")}" [fillcolor=white, style=filled ] \n"""
      case (acc: String, (name: String, node: Vertex)) if node.isInfected =>
        acc + s""" "${name.replace(" ", "\\n")}" [fillcolor=gray, style=filled ] \n"""
    })
    val edges = V.foldLeft("")({
      case (acc: String, (name: String, node: Vertex)) =>
      acc + node.neighbors.map((neighborName: String) => {
        if (neighborName > name)
          s""" "${name.replace(" ", "\\n")}" -- "${neighborName.replace(" ", "\\n")}"\n"""
        else
          ""
      }).fold("")(_ + _)
    })
    s"$header\n$options\n$nodes\n$edges\n$footer"
  }
}
