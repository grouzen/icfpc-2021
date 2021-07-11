package ua.scalapendra.icfpc2021

import io.circe.Decoder.Result
import io.circe._
import io.circe.generic.semiauto._
import scalafx.geometry.Point2D
import cats.syntax.either._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class Vector2D(start: Point, end: Point) {

  def squareLength: Double = start squareDistanceTo end

  def intersectedBy(edge: Vector2D): Boolean = {
    val o1 = Point.orientation(edge.start, edge.end, start)
    val o2 = Point.orientation(edge.start, edge.end, end)
    val o3 = Point.orientation(start, end, edge.start)
    val o4 = Point.orientation(start, end, edge.end)

    if (o1 == 0 && o2 == 0 &&
        Point.onEdge(start, edge.start, end) &&
        Point.onEdge(start, edge.end, end)) false
    else if (o1 == 0 && Point.onEdge(edge.start, start, edge.end)) false
    else if (o2 == 0 && Point.onEdge(edge.start, end, edge.end)) false
    else if (o3 == 0 && Point.onEdge(start, edge.start, end)) false
    else if (o4 == 0 && Point.onEdge(start, edge.end, end)) false
    else if (o1 != o2 && o3 != o4) true
    else false
  }

  def inHole(hole: Hole): Boolean =
    hole.edges.forall(!_.intersectedBy(this)) &&
      start.inHole(hole) &&
      end.inHole(hole)

}

object Vector2D {

  def shrinkAllowed(before: Vector2D, after: Vector2D, epsilon: Int): Boolean = {
    val ratio = epsilon.toDouble / 1000000
    math.abs((after.squareLength / before.squareLength) - 1) <= ratio
  }
}

case class Point(x: Int, y: Int) {
  override def toString: String = s"($x, $y)"

  def squareDistanceTo(that: Point): Double =
    math.pow(this.x - that.x, 2) + math.pow(this.y - that.y, 2)

  // https://www.geeksforgeeks.org/how-to-check-if-a-given-point-lies-inside-a-polygon/
  def intersectedWith(edge: Vector2D): Boolean = {
    val edgeMaxX = Math.max(edge.start.x, edge.end.x)
    val rayEnd   = if (x > edgeMaxX) x + 1 else edgeMaxX + 1
    val ray      = Vector2D(Point(x, y), Point(rayEnd, y))

    val o1 = Point.orientation(edge.start, edge.end, ray.start)
    val o2 = Point.orientation(edge.start, edge.end, ray.end)
    val o3 = Point.orientation(ray.start, ray.end, edge.start)
    val o4 = Point.orientation(ray.start, ray.end, edge.end)

    if (o1 != o2 && o3 != o4) true
    else if (o1 == 0 && Point.onEdge(edge.start, ray.start, edge.end)) true
    else if (o2 == 0 && Point.onEdge(edge.start, ray.end, edge.end)) true
    else if (o3 == 0 && Point.onEdge(ray.start, edge.start, ray.end)) true
    else if (o4 == 0 && Point.onEdge(ray.start, edge.end, ray.end)) true
    else false
  }

  def inHole(hole: Hole): Boolean = {
    val intersections = hole.edges.foldLeft(0.asRight[Boolean]) {
      case (countOrReturn, edge) =>
        countOrReturn.flatMap { count =>
          if (intersectedWith(edge)) {
            if (Point.orientation(edge.start, this, edge.end) == 0)
              Point.onEdge(edge.start, this, edge.end).asLeft
            else
              (count + 1).asRight
          } else count.asRight
        }
    }

    intersections match {
      case Right(v)     => v % 2 == 1
      case Left(result) => result
    }
  }

}

object Point {
  implicit val decoder: Decoder[Point] =
    Decoder[List[Int]].emap {
      case List(x, y) => Right(Point(x, y))
      case other      => Left(s"Expected array[2], got $other")
    }

  implicit val encoder: Encoder[Point] =
    Encoder[List[Int]].contramap[Point](p => List(p.x, p.y))

  def from2D(point: Point2D): Point = Point(point.x.toInt, point.y.toInt)

  def orientation(p1: Point, p2: Point, p3: Point): Int = {
    val result = (p2.y - p1.y) * (p3.x - p2.x) - (p2.x - p1.x) * (p3.y - p2.y)

    if (result == 0) 0     // colinear
    else if (result > 0) 1 // clockwise
    else 2                 // counterclockwise
  }

  def onEdge(p: Point, q: Point, r: Point): Boolean = {
    import Math._

    if (q.x <= max(p.x, r.x)
        && q.x >= min(p.x, r.x)
        && q.y <= max(p.y, r.y)
        && q.y >= min(p.y, r.y))
      true
    else
      false
  }

}

case class Figure(edges: List[Point], vertices: List[Point]) {

  val edgesV: List[Vector2D] =
    edges.map {
      case Point(start, end) =>
        Vector2D(vertices(start), vertices(end))
    }

  lazy val cycles: List[Figure] = {
    val graph: Map[Int, List[Int]] = {
      var base = Map.empty[Int, List[Int]]
      for (Point(u, v) <- edges) {
        base = base
          .updatedWith(u) {
            case None       => Some(List(v))
            case Some(list) => Some(list :+ v)
          }
          .updatedWith(v) {
            case None       => Some(List(u))
            case Some(list) => Some(list :+ u)
          }
      }
      base
    }

    println(graph)

    var cycleNumber = 0
    val cycles      = mutable.Map[Int, mutable.ListBuffer[Int]]()
    val N           = 100000
    for (i <- 0 until N) {
      cycles(i) = mutable.ListBuffer.empty[Int]
    }

    def dfsCycle(
      u:     Int,
      p:     Int,
      color: Array[Int],
      mark:  Array[Int],
      par:   Array[Int]
    ): Unit = {
      println(s"dfsCycle(u=$u, p=$p, ...)")
      if (color(u) == 2) {
        println(s"completely visited u=$u")
        return
      }
      if (color(u) == 1) {
        println(s"traversing cycle u=$u")
        cycleNumber += 1
        var cur     = p
        var prevCur = cur
        mark(cur) = cycleNumber

        while (cur != u) {
          prevCur = cur
          cur = par(u)
          if (prevCur == cur) {
            println(s"traversed cycle u=$u")
            return
          }
          mark(cur) = cycleNumber
        }
        println(s"traversed cycle u=$u")
        return
      }

      par(u) = p

      color(u) = 1
      for (v <- graph(u)) {
        if (v != par(u)) {
          println(s"deeper $u $v")
          dfsCycle(v, u, color, mark, par)
        }
      }

      color(u) = 2
    }

    def collectCycles(edges: Int, mark: Array[Int]): Map[Int, List[Int]] = {
      for (i <- 1 to edges) {
        if (mark(i) != 0) {
          println(s"Found cycle ${mark(i)}")
          cycles(mark(i)) += i
        }
      }
      cycles.map { case (idx, vs) => idx -> vs.toList }.toMap
    }

    val color = new Array[Int](N)
    val par   = new Array[Int](N)
    val mark  = new Array[Int](N)

    val edge = edges.length - 1
    dfsCycle(1, 0, color, mark, par)

    // todo: doesn't draw all edges
    collectCycles(edge, mark).map {
      case (x, ys) =>
        val newEdges = ys.map(y => Point(x, y))
//        val newVertices = newEdges.flatMap { case Point(i1, i2) => List(vertices(i1), vertices(i2)) }.distinct
        Figure(newEdges, vertices)
    }.toList
  }

  def inHole(hole: Hole): Boolean =
    edgesV.forall(_.inHole(hole))

}

object Figure {
  implicit val codec: Codec[Figure] = deriveCodec[Figure]
}

case class Hole(points: List[Point]) {

  assert(points.size >= 3)

  val edges: List[Vector2D] = {
    val first = points.head
    val last  = points.last

    Vector2D(first, last) :: points
      .sliding(2)
      .flatMap {
        case List(first, second) => Some(Vector2D(first, second))
        case _                   => None
      }
      .toList
  }

}

case class Problem(hole: Hole, figure: Figure, epsilon: Int)

object Problem {
  implicit val decoder: Decoder[Problem] = new Decoder[Problem] {
    override def apply(c: HCursor): Result[Problem] =
      for {
        holePoints <- c.downField("hole").as[List[Point]]
        epsilon    <- c.downField("epsilon").as[Int]
        figure     <- c.downField("figure").as[Figure]
      } yield Problem(Hole(holePoints), figure, epsilon)
  }

}

case class Pose(vertices: List[Point]) {

  def dislikes(problem: Problem): Int =
    problem.hole.points
      .map { holePoint =>
        val nearestVertex = vertices.minBy(_ squareDistanceTo holePoint)
        holePoint squareDistanceTo nearestVertex
      }
      .sum
      .toInt

}

object Pose {
  implicit val codec: Codec[Pose] = deriveCodec[Pose]
}
