package com.playground.strategy

import shapeless.{HNil, ::, HList}
import Common._
import scalaz._
import Scalaz._

object Default {

  /////////////////////////////////////////////////////////////////////////////////////////////////
  // This bias right and down movement if the right cell has not been visited before.
  /////////////////////////////////////////////////////////////////////////////////////////////////
  def processInputs(currentCell : String,
                    upCell      : String,
                    downCell    : String,
                    leftCell    : String,
                    rightCell   : String): State[Env, Unit] = modify { env =>
    val cells: HList          = (generateCell(currentCell)  ::
                                    generateCell(upCell)    ::
                                    generateCell(downCell)  ::
                                    generateCell(leftCell)  ::
                                    generateCell(rightCell) ::
                                    HNil)
    val x                     = env.currentPos.x
    val y                     = env.currentPos.y
    val top                   = Point(x, y - 1)
    val bottom                = Point(x, y + 1)
    val left                  = Point(x - 1, y)
    val right                 = Point(x + 1, y)
    ///////////////////////////////////////////////////////////////////////////////////////////////
    // Process the current
    ///////////////////////////////////////////////////////////////////////////////////////////////
    val preprocessed: Env     = {
      val currentPos = env.currentPos
      val unexplored = (env.unexplored - env.currentPos)
      val isNewCell  = !env.visited.toList.contains(currentPos)
      val result     = if (isNewCell && isCapitalLetter(currentCell)) {
        (env.result ++ List[Char](currentCell.charAt(0)))
      } else { env.result }
      val visited    = (env.visited + currentPos)
      val minTarget  = if (!env.unexplored.isEmpty) unexplored map {
        env.distance(env.currentPos, _)
      } else { Set.empty[Int] }
      val targetPos  = if (!minTarget.isEmpty) {
        val minTargetIndex = minTarget.zipWithIndex.minBy(_._1)._2
        Option(unexplored.toList(minTargetIndex))
      } else { None }
      (env.hasReachedTarget, env.hasNewCurrent) match {
        case (true, true)   =>
          env.copy(
            visited = visited,
            result = result,
            unexplored = unexplored,
            traceBack = List.empty[Point],
            targetPos = targetPos,
            hasStarted = true
          )
        case (true, false)  =>
          env.copy(
            visited = visited,
            result = result,
            unexplored = unexplored,
            traceBack = List.empty[Point],
            targetPos = targetPos,
            hasStarted = true
          )
        case (false, true)  =>
          env.copy(
            visited = visited,
            result = result,
            unexplored = unexplored,
            hasStarted = true
          )
        case (false, false) =>
          env.copy(
            visited = visited,
            result = result,
            unexplored = unexplored,
            hasStarted = true
          )
      }
    }
    //////////////////////////////////////////////////////////////////////////////////////////////
    // We greedily move to the unoccupied cell right -> left and build up the unexplored.
    //////////////////////////////////////////////////////////////////////////////////////////////
    val unexplored  = preprocessed.unexplored
    val visited     = preprocessed.visited
    val traceBack   = preprocessed.traceBack :+ env.currentPos
    val result: InProcessResult = cells match {
      case CharOrSpace(c) :: CharOrSpace(c1) :: CharOrSpace(c2) :: CharOrSpace(c3) :: CharOrSpace(c4) :: HNil
        if (env.surroundedByNewCells) =>
        println("R")
        val nextPos         = right
        val distinct        = (unexplored ++ Set[Point](top, bottom, left, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack)
      case CharOrSpace(c) :: CharOrSpace(c1) :: CharOrSpace(c2) :: _ :: CharOrSpace(c3) :: HNil
        if (env.hasNewTopBottomRight) =>
        println("R")
        val nextPos         = right
        val distinct        = (unexplored ++ Set[Point](top, bottom, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack)
      case CharOrSpace(c) :: CharOrSpace(c1) :: _ :: CharOrSpace(c2) :: CharOrSpace(c3) :: HNil
        if (env.hasNewTopLeftRight) =>
        println("R")
        val nextPos         = right
        val distinct        = (unexplored ++ Set[Point](top, left, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack)
      case CharOrSpace(c) :: CharOrSpace(c1) :: _ :: _ :: CharOrSpace(c3) :: HNil
        if (env.hasNewTopRight) =>
        println("R")
        val nextPos         = right
        val distinct        = (unexplored ++ Set[Point](top, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack)
      case CharOrSpace(c) :: _ :: CharOrSpace(c1) :: CharOrSpace(c2) :: CharOrSpace(c3) :: HNil
        if (env.hasNewBottomLeftRight) =>
        println("R")
        val nextPos         = right
        val distinct        = (unexplored ++ Set[Point](bottom, left, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack)
      case CharOrSpace(c) :: _ :: CharOrSpace(c1) :: _ :: CharOrSpace(c3) :: HNil
        if (env.hasNewBottomRight) =>
        println("R")
        val nextPos         = right
        val distinct        = (unexplored ++ Set[Point](bottom, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack)
      case CharOrSpace(c) :: _ :: _ :: CharOrSpace(c3) :: CharOrSpace(c4) :: HNil
        if (env.hasNewLeftRight) =>
        println("R")
        val nextPos         = right
        val distinct        = (unexplored ++ Set[Point](left, right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack)
      case CharOrSpace(c) :: _ :: _ :: _ :: CharOrSpace(c4) :: HNil
        if (env.hasNewRight) =>
        println("R")
        val nextPos         = right
        val distinct        = (unexplored ++ Set[Point](right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack)
      case CharOrSpace(c) :: CharOrSpace(c1) :: CharOrSpace(c2) :: CharOrSpace(c3) :: _ :: HNil
        if (env.hasNewTopBottomLeft) =>
        println("L")
        val nextPos         = left
        val distinct        = (unexplored ++ Set[Point](top, bottom, left)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack)
      case CharOrSpace(c) :: _ :: CharOrSpace(c2) :: CharOrSpace(c3) :: _ :: HNil
        if (env.hasNewBottomLeft) =>
        println("L")
        val nextPos         = left
        val distinct        = (unexplored ++ Set[Point](bottom, left)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack)
      case CharOrSpace(c) :: CharOrSpace(c1) :: _ :: CharOrSpace(c3) :: _ :: HNil
        if (env.hasNewTopLeft) =>
        println("L")
        val nextPos         = left
        val distinct        = (unexplored ++ Set[Point](top, left)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack)
      case CharOrSpace(c) :: _ :: _ :: CharOrSpace(c3) :: _ :: HNil
        if (env.hasNewLeft) =>
        println("L")
        val nextPos         = left
        val distinct        = (unexplored ++ Set[Point](left)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack)
      case CharOrSpace(c) :: CharOrSpace(c1) :: CharOrSpace(c2) :: _ :: _ :: HNil
        if (env.hasNewTopBottom) =>
        println("U")
        val nextPos         = top
        val distinct        = (unexplored ++ Set[Point](top, bottom)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack)
      case CharOrSpace(c) :: CharOrSpace(c1) :: _ :: _ :: _ :: HNil
        if (env.hasNewTop) =>
        println("U")
        val nextPos         = top
        val distinct        = (unexplored ++ Set[Point](top))  diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack)
      case CharOrSpace(c) :: _ :: CharOrSpace(c1) :: _ :: _ :: HNil
        if (env.hasNewBottom) =>
        println("D")
        val nextPos         = bottom
        val distinct        = (unexplored ++ Set[Point](bottom)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack)
      //////////////////////////////////////////////////////////////////////////////////////////////
      // We move to target if it is next to the current cell
      //////////////////////////////////////////////////////////////////////////////////////////////
      case CharOrSpace(c) :: CharOrSpace(c1) :: _ :: _ :: _ :: HNil
        if (env.targetIsToTop) =>
        println("U")
        val nextPos         = top
        val distinct        = (unexplored ++ Set[Point](top)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack)
      case CharOrSpace(c) :: _ :: CharOrSpace(c1)  :: _ :: _ :: HNil
        if (env.targetIsToBottom) =>
        println("D")
        val nextPos         = bottom
        val distinct        = (unexplored ++ Set[Point](bottom)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack)
      case CharOrSpace(c) :: _ :: _ :: CharOrSpace(c1) :: _ :: HNil
        if (env.targetIsToLeft) =>
        println("L")
        val nextPos         = left
        val distinct        = (unexplored ++ Set[Point](left)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack)
      case CharOrSpace(c) :: _ :: _ :: _ ::  CharOrSpace(c1) :: HNil
        if (env.targetIsToRight) =>
        println("R")
        val nextPos         = right
        val distinct        = (unexplored ++ Set[Point](right)) diff visited
        InProcessResult(nextPos, distinct, Option(nextPos), traceBack)
      //////////////////////////////////////////////////////////////////////////////////////////////
      // it has exhausted the cells around.  there is no new cell. we start to move towards
      // targetPos. We take all of the visited out of the unexplored as we go til it is
      // exhausted.
      //////////////////////////////////////////////////////////////////////////////////////////////
      case CharOrSpace(c) :: _ :: _ :: _ :: _ :: HNil =>
        findPath(preprocessed)
      case _ =>
        println("This is invalid state. It should not have happened. No movement.")
        InProcessResult(env.currentPos, unexplored, env.targetPos)
    }
    //////////////////////////////////////////////////////////////////////////////////////////////
    // we now processed and return.
    //////////////////////////////////////////////////////////////////////////////////////////////
    preprocessed.copy(
      currentPos            = result.nextPos,
      unexplored            = result.distinct,
      targetPos             = result.targetPos,
      traceBack             = result.traceBack
    )
  }

  //////////////////////////////////////////////////////////////////////////////////////////////
  // We just find our way back to the targetPos
  //////////////////////////////////////////////////////////////////////////////////////////////
  private def printMove(nextPos: Point, currentPos: Point): Unit = {
    if (nextPos isHigherThan currentPos) {
      println("U")
    } else if (nextPos isLowerThan currentPos) {
      println("D")
    } else if (nextPos isRightOf currentPos) {
      println("R")
    } else if (nextPos isLeftOf currentPos) {
      println("L")
    } else {
      println("This should not happen. nextPos is the same as currentPos")
    }
  }
  def findPath(env: Env): InProcessResult = {
    val currentPos          = env.currentPos
    val neighbors           = env.getNeighbors
    val targetPos           = env.targetPos
    val unexplored          = env.unexplored
    targetPos match {
      case Some(target) => {
        val steps = env.traceBack match {
          case Nil => env.getPathFromCurrentToTarget
          case _   => env.traceBack
        }
        val next: List[Point] = steps filter { env.isNeighbor(_) }
        val nextPos = next match {
          case Nil  => neighbors.head
          case _ if (neighbors.toList contains target) => target
          case _    => next.head
        }
        val traceBack = steps.dropWhile(p => p == nextPos || p == currentPos)
        printMove(nextPos, currentPos)
        InProcessResult(nextPos, unexplored, targetPos, traceBack)
      }
      case _ => {
        val nextPos   = (neighbors intersect env.visited).headOption.getOrElse(neighbors.headOption.getOrElse(currentPos))
        val targetPos = unexplored.headOption
        val traceBack = env.traceBack.dropWhile(p => p == nextPos || p == currentPos)
        printMove(nextPos, currentPos)
        InProcessResult(nextPos, unexplored, targetPos, traceBack)
      }
    }
  }
}

