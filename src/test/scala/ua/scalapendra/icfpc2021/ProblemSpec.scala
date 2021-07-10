package ua.scalapendra.icfpc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProblemSpec extends AnyFlatSpec with Matchers {

  it should "compute intersection correctly" in {
    val edge1 = Vector2D(Point(35, 5), Point(95, 95))
    val point1 = Point(45, 35)

    point1.intersectedWith(edge1) shouldBe true

    val edge2 = Vector2D(Point(5, 5), Point(35, 50))
    val point2 = Point(45, 35)

    point2.intersectedWith(edge2) shouldBe false

    val point3 = Point(90, 85)

    point3.intersectedWith(edge1) shouldBe false

    val point4 = Point(35, 50)

    point4.intersectedWith(edge2) shouldBe true

    val edge3 = Vector2D(Point(65, 95), Point(95, 95))
    val point5 = Point(80, 95)

    point5.intersectedWith(edge3) shouldBe true

    val edge4 = Vector2D(Point(5, 5), Point(45, 5))
    val point6 = Point(20, 5)

    point6.intersectedWith(edge4) shouldBe true
  }
}
