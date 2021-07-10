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
    val last  = points.last

    Vector2D(first, last) :: points
      .sliding(2)
      .flatMap {
        case List(first, second) => Some(Vector2D(first, second))
        case _                   => None
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

case class Point(x: Int, y: Int) { self =>
  override def toString: String = s"($x, $y)"

  final case class ~=(that: Point) {

    def delta(d: Double): Boolean =
      (math.abs(self.x - that.x) <= d &&
        math.abs(self.y - that.y) <= d) ||
        (math.abs(self.x + that.x) <= d &&
          math.abs(self.y + that.y) <= d)
  }

  def squareDistanceTo(that: Point): Double =
    math.pow(this.x - that.x, 2) + math.pow(this.y - that.y, 2)
}

object Point {
  implicit val decoder: Decoder[Point] =
    Decoder[List[Int]].emap {
      case List(x, y) => Right(Point(x, y))
      case other      => Left(s"Expected array[2], got $other")
    }

  implicit val encoder: Encoder[Point] =
    Encoder[List[Int]].contramap[Point](p => List(p.x, p.y))

  def from2D(point: Point2D, scale: Int, offset: Int): Point = Point(
    ((point.x - offset) / scale).toInt,
    ((point.y - offset) / scale).toInt
  )
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
