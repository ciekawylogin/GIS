package gis.graph


object State extends Enumeration {
  type State = Value
  val Susceptible, Infected = Value
}

case class Vertex(neighbors: Seq[String], state: State.Value = State.Susceptible) {
  lazy val degree =
    neighbors.size

  def withNeighbor(neighbor: String) =
    copy(neighbors = neighbors:+neighbor)

  lazy val susceptible =
    copy(state = State.Susceptible)

  lazy val infected =
    copy(state = State.Infected)

  lazy val isSusceptible =
    state == State.Susceptible

  lazy val isInfected =
    state == State.Infected
}
