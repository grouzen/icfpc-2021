package ua.scalapendra.icfpc2021

import io.circe.Decoder.Result
import io.circe._
import io.circe.generic.semiauto._
import scalafx.geometry.Point2D
import cats.syntax.either._

import java.util.concurrent.LinkedBlockingQueue
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.Await
import scala.concurrent.Future
import scala.jdk.CollectionConverters._

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

  def apply(idx: Int): Int =
    if (idx == 0) x
    else if (idx == 1) y
    else throw new UnsupportedOperationException(s"Point.apply($idx) not in [0, 1]")

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

  type Path   = Array[Int]
  type Cycles = Array[Path]

  lazy val cyclesNaive: List[Figure] = {
    val cycles = new LinkedBlockingQueue[Path]()

    def smallest(path: Path): Int = path.min
    def visited(n:     Int, path: Path): Boolean = path.contains(n)
    def isNew(path:    Path): Boolean =
      !cycles.contains(path)

    def normalize(path: Path): Path = {
      val p = new Array[Int](path.length)
      val x = smallest(path)
      var n = 0
      System.arraycopy(path, 0, p, 0, path.length)
      while (p(0) != x) {
        n = p(0)
        System.arraycopy(p, 1, p, 0, p.length - 1)
        p(p.length - 1) = n
      }
      p
    }

    def invert(path: Path): Path = path.reverse

    def findNewCycles(path: Path): Unit = {
      val n   = path(0)
      var x   = 0
      val sub = new Array[Int](path.length + 1)

      for (i <- edges.indices) {
        println(s"i=$i")
        var y = 0
        while (y <= 1) {
          println(s"y=$y")
          if (edges(i)(y) == n) {
            println("edge refers to our current node")
            x = edges(i)((y + 1) % 2)
            if (!visited(x, path)) {
              println(s"neighbor node not on path yet ($x, ${path.mkString("Array(", ", ", ")")})")
              sub(0) = x
              System.arraycopy(path, 0, sub, 1, path.length)
              findNewCycles(sub)
            } else if ((path.length > 2) && (x == path(path.length - 1))) {
              println(s"cycle found ($x, ${path.mkString("Array(", ", ", ")")})")
              val p   = normalize(path)
              val inv = invert(p)
              if (isNew(p) && isNew(inv)) {
                cycles.add(p)
              }
            }
          }

          y += 1
        }
      }
    }

    val resultsFuture = Future.traverse(edges.indices.grouped(8).toList) { is =>
      Future {
        for (i <- is) {
          for (j <- 0 to 1) {
            findNewCycles(Array(edges(i)(j)))
          }
        }
      }
    }

    Await.result(resultsFuture, Duration.Inf)

    cycles.asScala.toList.map { path =>
      val edges = path.toList
        .sliding(2)
        .flatMap {
          case List(x, y) => Some(Point(x, y))
          case _          => None
        }
        .toList

      Figure(edges, vertices)
    }
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
