package simulations

import math.random
import simulations.gui.EpidemyDisplay.Situation
import simulations.gui.EpidemyDisplay
import simulations.gui.Grid

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    val prevalenceRate = 0.01
    val transmissibility = 0.4
    val dyingRate = 0.25

    val maxDurationBeforeMove = 5

    val takePlaneProbability = 0

    val incubationLenght = 6
    val deathLength = 14
    val partialRecoveryLength = 16
    val totalRecoveryLength = 18
  }

  import SimConfig._

  val persons: List[Person] = (for(i <- 0 until population) yield new Person(i)).toList
  for (p <- persons) {
    val infectedOnStart = random < prevalenceRate
    if (infectedOnStart)
      p.infect()
  }

  for(p <- persons) p.enqueueNextTurn()

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)


    def enqueueNextTurn(): Unit =
      afterDelay(timeBeforeMoving) {
        if (!dead) {
          val p = nextPosition
          val hasMoved = p._1 != row || p._2 != col
          row = p._1
          col = p._2

          if (hasMoved && !infected && !immune)
            infectionPhase()

          enqueueNextTurn()
        }
      }

    private def timeBeforeMoving: Int =  randomBelow(maxDurationBeforeMove) + 1

    def nextPosition: (Int, Int) = {
      val takePlane = random < takePlaneProbability

      if (takePlane)
        (randomBelow(roomRows), randomBelow(roomColumns))
      else {
        val possibleMove = availableMove
        possibleMove(randomBelow(possibleMove.length))
      }
    }

    def availableMove: List[(Int, Int)] = {
      val left = (row , if (col == 0) roomColumns - 1 else col - 1)
      val up = (if (row == 0) roomRows - 1 else row - 1, col)
      val right = (row , (col + 1) % roomColumns)
      val down = ((row + 1) % roomRows, col)

      val available = List(left, up, right, down)
      val noSickPeopleInRooms = available filter (location => !hasSickPeopleInRoom(_.sick)(location))

      if (noSickPeopleInRooms.isEmpty)
        List((row, col))
      else
        noSickPeopleInRooms
    }

    def hasSickPeopleInRoom(predicate: Person => Boolean)(location: (Int, Int)) = location match {
      case (r,c) => persons.exists (p => p.row == r && p.col == c && predicate(p))
    }

    def infectionPhase() = {
      if (hasSickPeopleInRoom(_.infected)((row, col))) {
        val fallsSick = random < transmissibility
        if (fallsSick) {
          infect()
        }
      }
    }

    def infect() = {
      infected = true
      incubationPhase()
      finalPhase();
    }

    def incubationPhase() = afterDelay(incubationLenght) { sick = true }


    def finalPhase() = {
      afterDelay(deathLength) {
        val shouldDie = random < dyingRate
        if (shouldDie)
          dead = true
        else {
          recoveryPhase()
        }
      }
    }

    def recoveryPhase() = {
      afterDelay(partialRecoveryLength - deathLength) {
        immune = true
        sick = false
      }

      afterDelay(totalRecoveryLength - deathLength) {
        immune = false
        infected = false
      }
    }
  }

}
