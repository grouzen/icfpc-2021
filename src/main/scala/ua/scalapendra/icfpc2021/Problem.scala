package ua.scalapendra.icfpc2021

import io.circe._
import io.circe.generic.semiauto._
import scalafx.geometry.Point2D

case class Vector2D(start: Point, end: Point) {
  def squareLength: Double = start squareDistanceTo end
}

object Vector2D {

  def mkVectors(points: List[Point]): List[Vector2D] = {
    val first = points.head
    val last = points.last

    Vector2D(first, last) :: points
      .sliding(2)
      .flatMap {
        case List(first, second) => Some(Vector2D(first, second))
        case _ => None
      }
      .toList
  }

  def mkVectors(figure: Figure): List[Vector2D] =
    figure.edges.map {
      case Point(start, end) =>
        Vector2D(figure.vertices(start), figure.vertices(end))
    }

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

    def orientation(p1: Point, p2: Point, p3: Point): Int = {
      val result = (p2.y - p1.y) * (p3.x - p2.x) - (p2.x - p1.x) * (p3.y - p2.y)

      if (result == 0) 0 // colinear
      else if (result > 0) 1 // clockwise
      else 2 // counterclockwise
    }

    def onEdge(p: Point, q: Point, r: Point): Boolean = {
      import Math._

      if (q.x <= max(p.x, r.x) && q.x >= min(p.x, r.x) && q.y <= max(p.y, r.y) && q.y >= min(p.y, r.y)) true
      else false
    }

    val edgeMaxX = Math.max(edge.start.x, edge.end.x)
    val rayEnd = if (x > edgeMaxX) x + 1 else edgeMaxX + 1
    val ray = Vector2D(Point(x, y), Point(rayEnd, y))

    val o1 = orientation(edge.start, edge.end, ray.start)
    val o2 = orientation(edge.start, edge.end, ray.end)
    val o3 = orientation(ray.start, ray.end, edge.start)
    val o4 = orientation(ray.start, ray.end, edge.end)

    if (o1 != o2 && o3 != o4) true
    else if (o1 == 0 && onEdge(edge.start, ray.start, edge.end)) true
    else if (o2 == 0 && onEdge(edge.start, ray.end, edge.end)) true
    else if (o3 == 0 && onEdge(ray.start, edge.start, ray.end)) true
    else if (o4 == 0 && onEdge(ray.start, edge.end, ray.end)) true
    else false
  }

}

object Point {
  implicit val decoder: Decoder[Point] =
    Decoder[List[Int]].emap {
      case List(x, y) => Right(Point(x, y))
      case other => Left(s"Expected array[2], got $other")
    }

  implicit val encoder: Encoder[Point] =
    Encoder[List[Int]].contramap[Point](p => List(p.x, p.y))

  def from2D(point: Point2D): Point = Point(point.x.toInt, point.y.toInt)
}

case class Figure(edges: List[Point], vertices: List[Point])

object Figure {
  implicit val codec: Codec[Figure] = deriveCodec[Figure]
}

case class Problem(hole: List[Point], figure: Figure, epsilon: Int)

object Problem {
  implicit val codec: Codec[Problem] = deriveCodec[Problem]
}

case class Pose(vertices: List[Point])

object Pose {

  def dislikes(pose: Pose, problem: Problem): Int =
    problem.hole
      .map { hole =>
        val nearestVertex = pose.vertices.minBy(_ squareDistanceTo hole)
        hole squareDistanceTo nearestVertex
      }
      .sum
      .toInt

  implicit val codec: Codec[Pose] = deriveCodec[Pose]
}
