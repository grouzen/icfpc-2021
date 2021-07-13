package ua.scalapendra.icfpc2021.solver

import ua.scalapendra.icfpc2021.{Pose, Problem}
import io.circe.syntax._

import java.nio.file.{Files, Path}

trait SolverLike {

  val problem: Problem
  val pose: Pose

  def dumpSolution(name: String): Unit = {
    val bytes = pose.asJson.toString.getBytes

    Files.write(Path.of("/tmp", name), bytes)
  }

}
