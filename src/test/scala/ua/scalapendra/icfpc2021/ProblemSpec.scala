package ua.scalapendra.icfpc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProblemSpec extends AnyFlatSpec with Matchers {

  it should "compute intersection correctly" in {
    val edge1  = Vector2D(Point(35, 5), Point(95, 95))
    val point1 = Point(45, 35)

    point1.intersectedWith(edge1) shouldBe true

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

    val edge4  = Vector2D(Point(5, 5), Point(45, 5))
    val point7 = Point(20, 5)

    point7.intersectedWith(edge4) shouldBe true

    val edge5 = Vector2D(Point(95, 95), Point(65, 95))

    point6.intersectedWith(edge5) shouldBe true
  }

  it should "compute inHold correctly for given points" in {
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

}
