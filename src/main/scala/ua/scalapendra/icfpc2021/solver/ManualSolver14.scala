package ua.scalapendra.icfpc2021.solver

import ua.scalapendra.icfpc2021.{Pose, Problem}

case class ManualSolver14(problem: Problem) extends SolverLike {

  override val pose: Pose =
    Pose(problem.figure.rotate(48).translate(1, -1).vertices)

}
