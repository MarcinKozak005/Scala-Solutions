package P03_Intermediate

class P03Graph extends App {
}


class Graph private(_values: Map[String, Option[Int]],
                    _inEdges: Map[String, List[Edge]],
                    _outEdges: Map[String, List[Edge]]) {
  type Vertex = String
  private val vertices = _values
  private val inEdges = _inEdges
  private val outEdges = _outEdges

  def addVertex(name: Vertex, value: Option[Int] = None): Graph =
    if (vertices.keySet.contains(name)) this
    else Graph(
      vertices.updated(name, value),
      inEdges.updated(name, List[Edge]()),
      outEdges.updated(name, List[Edge]()),
    )

  def getVertexValue(name: Vertex): Option[Int] = vertices.getOrElse(name, None)

  def updateVertexValue(name: Vertex, value: Option[Int]): Graph =
    Graph(vertices.updated(name, value), inEdges, outEdges)

  def removeVertex(name: Vertex): Graph = Graph(
    vertices.removed(name),
    inEdges.removed(name),
    outEdges.removed(name)
  )

  def addDirectedConnection(from: Vertex, to: Vertex, value: Option[Int] = None): Graph =
    this
      .addVertex(from)
      .addVertex(to)
      .addOutEdge(from, to, value)
      .addInEdge(from, to, value)

  def addUndirectedConnection(v1: Vertex, v2: Vertex, value: Option[Int] = None): Graph =
    this
      .addDirectedConnection(v1, v2, value)
      .addDirectedConnection(v2, v1, value)

  private def addInEdge(from: Vertex, to: Vertex, value: Option[Int] = None) = Graph(
    vertices,
    inEdges.updated(to, Edge(from, to, value) :: inEdges.getOrElse(to, List())),
    outEdges,
  )

  private def addOutEdge(from: Vertex, to: Vertex, value: Option[Int] = None) = Graph(
    vertices,
    inEdges,
    outEdges.updated(from, Edge(from, to, value) :: outEdges.getOrElse(from, List())),
  )

}

object Graph {
  type Vertex = String

  def apply() =
    new Graph(
      Map[Vertex, Option[Int]](),
      Map[Vertex, List[Edge]](),
      Map[Vertex, List[Edge]](),
    )

  def apply(values: Map[String, Option[Int]],
            inEdges: Map[String, List[Edge]],
            outEdges: Map[String, List[Edge]],
           ) =
    new Graph(values, inEdges, outEdges)
}

private class Edge private(val from: String, val to: String, val value: Option[Int]) {}

object Edge {
  type Vertex = String

  def apply(from: Vertex, to: Vertex, value: Option[Int]): Edge = new Edge(from, to, value)
}