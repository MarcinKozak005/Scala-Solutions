package P03_Intermediate

import P03_Intermediate.Graph.{Edge, Vertex}

import scala.annotation.tailrec

class P03Graph extends App {
}


class Graph private(vertices: Map[Vertex, Option[Int]],
                    edges: Map[Edge, Option[Int]],
                    inEdges: Map[Vertex, Set[Edge]],
                    outEdges: Map[Vertex, Set[Edge]],
                    toVertices: Map[Edge, Set[Vertex]],
                    fromVertices: Map[Edge, Set[Vertex]],
                   ) {
  def setVertex(name: Vertex, value: Option[Int] = None): Graph =
    Graph(
      vertices.updated(name, value),
      edges,
      inEdges.updated(name, Set[Edge]()),
      outEdges.updated(name, Set[Edge]()),
      toVertices,
      fromVertices,
    )

  def getVertexValue(name: Vertex): Option[Int] = vertices.getOrElse(name, None)

  def removeVertex(name: Vertex): Graph =
    Graph(
      vertices.removed(name),
      edges,
      inEdges.removed(name),
      outEdges.removed(name),
      toVertices,
      fromVertices,
    )

  def setDirectedConnection(from: Vertex, to: Vertex, edge: Edge, value: Option[Int] = None): Graph =
    addDirectedConnectionHelper(from, to, s"d_$edge", value)

  def setUndirectedConnection(v1: Vertex, v2: Vertex, edge: Edge, value: Option[Int] = None): Graph =
    this
      .addDirectedConnectionHelper(v1, v2, s"u_$edge", value)
      .addDirectedConnectionHelper(v2, v1, s"u_$edge", value)

  def removeEdge(edge: Edge): Graph = Graph(
    vertices,
    edges.removed(edge),
    removeEdgeFromVertices(inEdges, toVertices.getOrElse(edge, Set[Vertex]()).toList, edge),
    removeEdgeFromVertices(outEdges, fromVertices.getOrElse(edge, Set[Vertex]()).toList, edge),
    toVertices.removed(edge),
    fromVertices.removed(edge),
  )
  @tailrec
  private def removeEdgeFromVertices(collection: Map[Vertex, Set[Edge]], verticesList: List[Vertex], edge: Edge): Map[Vertex, Set[Edge]] = verticesList match {
    case v::tail => removeEdgeFromVertices(collection.updated(v,collection.getOrElse(v,Set[Edge]()).excl(edge)),tail,edge)
    case Nil => collection
  }

  private def addDirectedConnectionHelper(from: Vertex, to: Vertex, edge: Edge, value: Option[Int]): Graph = {
    this
      .setVertex(from, getVertexValue(from))
      .setVertex(to, getVertexValue(to))
      .addOutEdge(from, edge, value)
      .addInEdge(to, edge, value)
  }

  private def addInEdge(to: Vertex, edge: Edge, value: Option[Int] = None) = Graph(
    vertices,
    edges.updated(edge, value),
    inEdges.updated(to, edge :: inEdges.getOrElse(to, Set())),
    outEdges,
    toVertices.updated(edge, to :: toVertices.getOrElse(edge, Set())),
    fromVertices,
  )

  private def addOutEdge(from: Vertex, edge: Edge, value: Option[Int] = None) = Graph(
    vertices,
    edges.updated(edge, value),
    inEdges,
    outEdges.updated(from, from :: outEdges.getOrElse(from, Set())),
    toVertices,
    fromVertices.updated(edge, from :: fromVertices.getOrElse(edge, Set())),
  )

}

object Graph {
  type Vertex = String
  type Edge = String

  def apply() =
    new Graph(
      Map[Vertex, Option[Int]](),
      Map[Edge, Option[Int]](),
      Map[Vertex, Set[Edge]](),
      Map[Vertex, Set[Edge]](),
      Map[Edge, Set[Vertex]](),
      Map[Edge, Set[Vertex]](),
    )

  private def apply(vertices: Map[Vertex, Option[Int]],
                    edges: Map[Edge, Option[Int]],
                    inEdges: Map[Vertex, Set[Edge]],
                    outEdges: Map[Vertex, Set[Edge]],
                    toVertices: Map[Edge, Set[Vertex]],
                    fromVertices: Map[Edge, Set[Vertex]]) =
    new Graph(vertices, edges, inEdges, outEdges, toVertices, fromVertices)
}
