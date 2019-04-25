package com.playground.solution

import scalaz._
import Scalaz._
import scalaz.stream._
import scalaz.concurrent.Task
import scala.language.postfixOps
import com.playground.strategy._
import Common._
import Default._

/***************************************************************************************
  *
  * The strategy is to move to the topleft to orient the agent.  Once it pins one
  * cell as the topleft, it will start moving to the right, left and down when it hits
  * the wall. Thus it will scan the space for all the characters found.
  *
  * It will bias up movement if the up cell has not been visited before.
  * If any new cell is higher or to the left of the topleft, it will start
  * over again from the new topleft cell.
  *
  * When it is done getting new cells, the agent will go back to the unexplored and
  * search out for those until it runs out of the unexplored and calls it done!
  *************************************************************************************/


object Main {

  sealed case class End(message: String)

  def main(args: Array[String]): Unit   = {
    task.run match {
      case Some(env) => {
        val result = ((env.result filter { c => isCapitalLetter (c.toString) }).sorted).mkString
        if (!result.isEmpty)
          println (s"""The result is K${result}""")
        else ()
      }
      case _ => println("Error: The environment was not set up properly and the application can not be run!")
    }
  }

  val task: Task[Option[Env]] =
    io.stdInLines
      .takeWhile(!_.toLowerCase.startsWith("k"))
      .scan(initial)((accum, str) => {
        str.length match {
          case 5 => {
            val Array(current, up, down, left, right) = str.toArray.map(_.toString)
            if (!isValidInput(current)  || !isValidInput(up) || !isValidInput(down) || !isValidInput(left) || !isValidInput(right)) {
              Console.out.println("Input contains invalid characters. Valid charaters A-Z, <space>, <line feed> and #")
              accum
            } else {
              val e  = processInputs(current, up, down, left, right).exec(accum)
              e
            }
          }
          case _ => {
            Console.out.println("Command input is not 5 characters.")
            accum
          }
        }
      })
      .map(checkEndCondition)
      .map(logSignal)
      .runLast

  def checkEndCondition(env: Env): End \/ Env = {
    val result          = ((env.result filter { c => isCapitalLetter(c.toString) }).sorted)
    (env.hasStarted && env.unexplored.isEmpty) match {
      case true => {
        -\/(End(s"The agent seems to have completed the grid. Please start again with a new session. The result is K${result.mkString}"))
      }
      case _ => {
        \/-(env)
      }
    }
  }

  def logSignal: PartialFunction[End \/ Env, Env] = {
    case -\/(end)     => println(end.message); initial
    case \/-(env)     => env
  }
}