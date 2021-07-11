package ua.scalapendra.icfpc2021

import io.circe.parser
import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.geometry.Point2D
import scalafx.scene.Scene
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color._
import scalafx.scene.paint._
import scalafx.scene.shape.{Circle, Line}
import scalafx.scene.text.Font
import scalafx.scene.text.Text

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends JFXApp3 {

  private val Scale  = 10
  private val Offset = 200

  override def start(): Unit =
    stage = new PrimaryStage {
      title = "ICFPC 2021 visualizer"
      scene = visualizeProblem(
        readProblemFromFile(Paths.get("problems", "14.problem"))
      )
    }

  private def readProblemFromFile(file: Path): Problem =
    parser
      .decode[Problem](new String(Files.readAllBytes(file)))
      .fold(throw _, identity)

  private def readPoseFromFile(file: Path): Pose =
    parser
      .decode[Pose](new String(Files.readAllBytes(file)))
      .fold(throw _, identity)

  class FigureInteractor(problem: Problem) {
    val lines = mkLines(problem.figure.edgesV, Color.Red)

    private var _pose: Pose = Pose(problem.figure.vertices)

    def pose: Pose = _pose

    def pose_=(p: Pose): Unit = {
      _pose = p
      val newFigure = problem.figure.copy(vertices = _pose.vertices)
      for ((line, idx) <- newFigure.edgesV.zipWithIndex)
        updateLines(idx, line.start, line.end)
    }

    private val dislikeTextPane = new Text(600, 300, "") {
      text = dislikesTest
      font = Font(48)
      fill = new LinearGradient(endX = 0, stops = Stops(Red, DarkRed))
    }

    val scores = Seq(dislikeTextPane)

    private def dislikesTest: String = {
      val dislikes = pose.dislikes(problem)
      s"Dislikes: $dislikes"
    }

    private val verticeToLine = lines.zipWithIndex.map(_.swap).toMap

    private def updateLines(idx: Int, start: Point, end: Point): Unit = {
      val line = verticeToLine(idx)

      line.startX = start.x * Scale + Offset
      line.startY = start.y * Scale + Offset

      line.endX = end.x * Scale + Offset
      line.endY = end.y * Scale + Offset

      dislikeTextPane.text = dislikesTest
    }

  }

  private def mkLines(vectors: List[Vector2D], color: Color): List[Line] = {

    def mkLine(vector: Vector2D): Line =
      new Line {
        stroke = color
        strokeWidth = 3
        startX = vector.start.x * Scale + Offset
        startY = vector.start.y * Scale + Offset
        endX = vector.end.x * Scale + Offset
        endY = vector.end.y * Scale + Offset
      }
    vectors.map(mkLine)
  }

  private def visualizeProblem(problem: Problem) = {
    val holes            = problem.hole.edges
    val figureInteractor = new FigureInteractor(problem)
    val centroid         = Point.centroid(problem.figure.vertices)

    val scene = new Scene(1000, 800) {
      fill = Color.White
      content = new Pane {
        children = mkLines(holes, Color.Black) ++
          figureInteractor.lines ++
          figureInteractor.scores :+
          Circle(
            centroid.x.toDouble * Scale + Offset,
            centroid.y.toDouble * Scale + Offset,
            5f
          )
      }
    }
    Future {
      Thread.sleep(1000)

      val modified = problem.figure.rotate(48).translate(1, -1)
      figureInteractor.pose = Pose(modified.vertices)
    }
    scene
  }
}
