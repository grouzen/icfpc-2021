package ua.scalapendra.icfpc2021

import io.circe.parser
import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.geometry.Point2D
import scalafx.scene.Scene
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color._
import scalafx.scene.paint._
import scalafx.scene.shape.Line
import scalafx.scene.text.Font
import scalafx.scene.text.Text
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

object Main extends JFXApp3 {

  private val Scale  = 3
  private val Offset = 200

  override def start(): Unit =
    stage = new PrimaryStage {
      title = "ICFPC 2021 visualizer"
      scene = visualizeProblem(
        readProblemFromFile(Paths.get("problems", "1.problem")),
        readPoseFromFile(Paths.get("solutions", "1.solution"))
      )
    }

  private def readProblemFromFile(file: Path): Problem =
    parser
      .decode[Problem](
        new String(Files.readAllBytes(file))
      )
      .fold(throw _, identity)

  private def readPoseFromFile(file: Path): Pose =
    parser
      .decode[Pose](
        new String(Files.readAllBytes(file))
      )
      .fold(throw _, identity)

  class FigureInteractor(initial: Figure) extends ShapeDrawInteractor {
    private var figure = initial
    def lines          = mkLines(Vector2D.mkVectors(figure), Color.Red)

    override def update(): Unit = {
      val started  = Point.from2D(start)
      val pointIdx = figure.vertices.indexOf(started, from = 0)
      if (pointIdx >= 0) {
        figure = figure.copy(
          vertices = figure.vertices.updated(pointIdx, Point.from2D(end))
        )
      }
    }
  }

  private def mkLines(vectors: List[Vector2D], color: Color): List[Line] = {

    def mkLine(vector: Vector2D): Line = new Line {
      stroke = color
      strokeWidth = 3
      startX = vector.start.x * Scale + Offset
      startY = vector.start.y * Scale + Offset
      endX = vector.end.x * Scale + Offset
      endY = vector.end.y * Scale + Offset
    }

    vectors.map(mkLine)
  }

  private def visualizeProblem(problem: Problem, pose: Pose) = {
    val holes            = Vector2D.mkVectors(problem.hole)
    val figureInteractor = new FigureInteractor(problem.figure)

    val dislikes = Pose.dislikes(pose, problem)

    new Scene(1000, 800) {
      fill = Color.White
      content = new Pane {
        children = mkLines(holes, Color.Black) ++
          figureInteractor.lines ++
          Seq(
            new Text(600, 300, s"Dislikes: $dislikes") {
              font = Font(48)
              fill = new LinearGradient(
                endX = 0,
                stops = Stops(Red, DarkRed)
              )
            }
          )
      }
    }
  }

  trait MouseHandler { def handler: MouseEvent => Unit }

  trait ShapeDrawInteractor extends MouseHandler {
    private var _start = new Point2D(0, 0)
    private var _end   = new Point2D(0, 0)
    def start: Point2D = _start

    def start_=(p: Point2D): Unit = {
      _start = p
      _end = p
      update()
    }
    def end: Point2D = _end

    def end_=(p: Point2D): Unit = {
      _end = p
      update()
    }

    def update(): Unit

    override def handler: MouseEvent => Unit = { (me: MouseEvent) =>
      me.eventType match {
        case MouseEvent.MousePressed =>
          start = new Point2D(me.x, me.y)
        case MouseEvent.MouseDragged =>
          end = new Point2D(me.x, me.y)
        case _ =>
      }
    }
  }
}
