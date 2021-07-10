package ua.scalapendra.icfpc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProblemSpec extends AnyFlatSpec with Matchers {

  it should "compute intersection between edges correctly" in {
    val edge1 = Vector2D(Point(35, 50), Point(5, 95))
    val edge2 = Vector2D(Point(10, 60), Point(30, 80))
    val edge3 = Vector2D(Point(15, 55), Point(25, 65))
    val edge4 = Vector2D(Point(15, 55), Point(20, 60))
    val edge5 = Vector2D(Point(15, 80), Point(35, 50))

    edge1.intersectedBy(edge2) shouldBe true
    edge1.intersectedBy(edge3) shouldBe false
    edge1.intersectedBy(edge4) shouldBe false
    edge1.intersectedBy(edge5) shouldBe false

    val edge6 = Vector2D(Point(5, 5), Point(35, 5))
    val edge7 = Vector2D(Point(10, 5), Point(30, 5))
    val edge8 = Vector2D(Point(10, 5), Point(30, 10))

    edge6.intersectedBy(edge7) shouldBe false
    edge6.intersectedBy(edge8) shouldBe false
  }

  it should "compute intersection of given points with given edges correctly" in {
    val edge1   = Vector2D(Point(35, 5), Point(95, 95))
    val point1  = Point(45, 35)
    val point22 = Point(75, 65)

    point1.intersectedWith(edge1) shouldBe true
    point22.intersectedWith(edge1) shouldBe true

    val edge2  = Vector2D(Point(5, 5), Point(35, 50))
    val point2 = Point(45, 35)
    val point3 = Point(90, 85)
    val point4 = Point(35, 50)

    point2.intersectedWith(edge2) shouldBe false
    point3.intersectedWith(edge1) shouldBe false
    point4.intersectedWith(edge2) shouldBe true

    val edge3  = Vector2D(Point(65, 95), Point(95, 95))
    val point5 = Point(80, 95)
    val point6 = Point(95, 95)

    point5.intersectedWith(edge3) shouldBe true
    point6.intersectedWith(edge3) shouldBe true

    val edge4  = Vector2D(Point(5, 5), Point(35, 5))
    val point7 = Point(20, 5)

    point7.intersectedWith(edge4) shouldBe true

    val edge5 = Vector2D(Point(95, 95), Point(65, 95))

    point6.intersectedWith(edge5) shouldBe true
  }

  it should "compute Point.onEdge for given points colinear with edges correctly" in {
    val edge1  = Vector2D(Point(35, 5), Point(95, 95))
    val point1 = Point(65, 50)
    val point2 = Point(75, 65)
    val point3 = Point(105, 110)

    Point.onEdge(edge1.start, point1, edge1.end) shouldBe true
    Point.onEdge(edge1.start, point2, edge1.end) shouldBe true
    Point.onEdge(edge1.start, point3, edge1.end) shouldBe false
  }

  it should "compute inHole correctly for given points correctly" in {
    val hole = Hole(
      List(
        Point(45, 80),
        Point(35, 95),
        Point(5, 95),
        Point(35, 50),
        Point(5, 5),
        Point(35, 5),
        Point(95, 95),
        Point(65, 95),
        Point(55, 80)
      )
    )

    val point1 = Point(45, 65)
    val point2 = Point(50, 15)
    val point3 = Point(15, 95)
    val point4 = Point(65, 50)
    val point5 = Point(65, 45)
    val point6 = Point(5, 5)
    val point7 = Point(95, 95)

    point1.inHole(hole) shouldBe true
    point2.inHole(hole) shouldBe false
    point3.inHole(hole) shouldBe true
    point4.inHole(hole) shouldBe true
    point5.inHole(hole) shouldBe false
    point6.inHole(hole) shouldBe true
    point7.inHole(hole) shouldBe true
  }

  it should "compute inHole for given edges correctly" in {
    val hole = Hole(
      List(
        Point(45, 80),
        Point(35, 95),
        Point(5, 95),
        Point(35, 50),
        Point(5, 5),
        Point(35, 5),
        Point(95, 95),
        Point(65, 95),
        Point(55, 80)
      )
    )

    val edge1 = Vector2D(Point(45, 45), Point(45, 65))
    val edge2 = Vector2D(Point(55, 25), Point(55, 50))
    val edge3 = Vector2D(Point(55, 80), Point(85, 80))
    val edge4 = Vector2D(Point(55, 80), Point(90, 80))
    val edge5 = Vector2D(Point(35, 80), Point(60, 80))

    edge1.inHole(hole) shouldBe true
    edge2.inHole(hole) shouldBe false
    edge3.inHole(hole) shouldBe true
    edge4.inHole(hole) shouldBe false
    edge5.inHole(hole) shouldBe true
  }

}
