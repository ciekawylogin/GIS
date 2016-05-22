package gis.graph


object State extends Enumeration {
  type State = Value
  val Susceptible, Infected = Value
}

case class Vertex(neighbors: Seq[String], state: State.Value = State.Susceptible) {
  def degree() =
    neighbors.size

  def withNeighbor(neighbor: String) =
    copy(neighbors = neighbors:+neighbor)

  lazy val susceptible =
    copy(state = State.Susceptible)

  lazy val infected =
    copy(state = State.Infected)
}
