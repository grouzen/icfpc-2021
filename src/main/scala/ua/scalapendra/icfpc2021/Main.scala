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
import javafx.scene.{input => jfxi}
import javafx.{event => jfxe}

object Main extends JFXApp3 {

  private val Scale = 3
  private val Offset = 200

  override def start(): Unit =
    stage = new PrimaryStage {
      title = "ICFPC 2021 visualizer"
      scene = visualizeProblem(
        readProblemFromFile(Paths.get("problems", "1.problem"))
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

  class FigureInteractor(problem: Problem, pane: Pane) extends MouseHandler {
    private var pose = Pose(problem.figure.vertices)

    val lines =
      mkLines(Vector2D.mkVectors(problem.figure), Color.Red)
        .map { line =>
          pane.onMouseClicked = jfxiHandler
          line
        }

    private val dislikeTextPane = new Text(600, 300, "") {
      text = dislikesTest
      font = Font(48)
      fill = new LinearGradient(endX = 0, stops = Stops(Red, DarkRed))
    }

    val scores = Seq(dislikeTextPane)

    private def dislikesTest: String = {
      val dislikes = Pose.dislikes(pose, problem)
      s"Dislikes: $dislikes"
    }

    private val verticeToLine = lines.zipWithIndex.map(_.swap).toMap

    private def updateLines(idx: Int, isStarted: Boolean): Unit = {
      val line = verticeToLine(idx)
      val point = pose.vertices(idx)
      if (isStarted) {
        line.startX = point.x
        line.startY = point.y
      } else {
        line.endX = point.x
        line.endY = point.y
      }
      dislikeTextPane.text = dislikesTest
    }

    // todo: update doesn't work correctly (target point is set to deto v yebenyiax)
    private def update(start: Point2D, end: Point2D): Unit = {
      val started = Point.from2D(start, Scale, Offset)
      val ended = Point.from2D(end, Scale, Offset)
      println(s"started=$started ended=$ended")
      findAprox(pose.vertices, delta = 5)(started) match {
        case None =>
          println("No update")
        case Some((pointIdx, isStarted, p)) =>
          println(s"Found $pointIdx $p")
          pose = pose.copy(vertices = pose.vertices.updated(pointIdx, ended))
          updateLines(pointIdx, isStarted)
          startOpt = None
      }
    }

    private var startOpt = Option.empty[Point2D]

    private def setStart(p: Point2D): Unit = {
      println(s"setStart($p)")
      startOpt = Some(p)
    }

    override def handler: MouseEvent => Unit = { (me: MouseEvent) =>
      me.eventType match {
        case MouseEvent.MouseClicked if startOpt.isEmpty =>
          // todo: check that clicked position is vertice
          setStart(new Point2D(me.x, me.y))
        case MouseEvent.MouseClicked if startOpt.nonEmpty =>
          update(startOpt.get, new Point2D(me.x, me.y))
        case _ =>
      }
    }

    private def findAprox(points: List[Point], delta: Double)(
      point: Point
    ): Option[(Int, Boolean, Point)] =
      points.zipWithIndex
        .sliding(2)
        .foldLeft(Option.empty[(Int, Boolean, Point)]) {
          case (res @ Some(_), _) => res
          case (None, List((start, startIdx), (end, endIdx))) =>
            Option
              .when((point ~= start).delta(delta))((startIdx, true, start))
              .orElse {
                Option.when((point ~= end).delta(delta))((endIdx, false, end))
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

  private def visualizeProblem(problem: Problem) = {
    val holes = Vector2D.mkVectors(problem.hole)
    new Scene(1000, 800) {
      fill = Color.White
      content = new Pane {
        val figureInteractor = new FigureInteractor(problem, this)
        children = mkLines(holes, Color.Black) ++
          figureInteractor.lines ++
          figureInteractor.scores
      }
    }
  }

  trait MouseHandler {
    protected def handler: MouseEvent => Unit

    def jfxiHandler: jfxe.EventHandler[_ >: jfxi.MouseEvent] =
      (e: jfxi.MouseEvent) => {
        println(s"Got event $e")
        handler(new MouseEvent(e))
      }
  }
}
