package peg

import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import scala.collection.mutable.ListBuffer

import Elements._
import Directions._

class Position(val x: Int) {
  def +(that: Position) = {
    new Position(x + that.x)
  }
  
  def -(that: Position) = {
    new Position(x - that.x)
  }
  
  def inBounds(gameBoard: Seq[AnyRef]): Boolean = {
    x >= 0 && x < gameBoard.length
  }
  
  override def toString = s"x: $x"
}

class GameState(val gameBoard: Array[Elements], val parent: GameState = null) {
  override def equals(o: Any) = o match {
    case that: GameState => {
      that.gameBoard.sameElements(this.gameBoard)
    } 
    case _ => false
  }
  
  override def hashCode = {
    gameBoard.##
  }
  
  override def toString = "GameState: " + gameBoard.mkString(" ")
}

object Rules {
    val initialBoard = Array[Elements](Peg, Peg, Peg, Peg, Hole, Peg, Peg, Peg, Peg, Peg)
    
    def applyJump(gameBoard: Array[Elements], pegPosition: Position, targetPosition: Position): Array[Elements] = {
      if (!targetPosition.inBounds(gameBoard) || !pegPosition.inBounds(gameBoard)) {
        throw new RuntimeException(s"Invalid positions peg: $pegPosition target: $targetPosition")
      }
      val middleX = (targetPosition.x + pegPosition.x) / 2
      Array.tabulate(gameBoard.length)(x => {
        x match {
          case pegPosition.x => Hole
          case `middleX` => Hole
          case targetPosition.x => Peg
          case _ => gameBoard(x)
        }
      })
    }
    
    def applyJumpRelative(gameBoard: Array[Elements], pegPosition: Position, relativePosition: Position): Array[Elements] = {
      applyJump(gameBoard, pegPosition, pegPosition + relativePosition)
    }
    
    def applyJumpDirected(gameBoard: Array[Elements], pegPosition: Position, direction: Directions): Array[Elements] = {
      val curriedApply = applyJump(gameBoard, pegPosition, _: Position)
      direction match {
        case Left => curriedApply(pegPosition - new Position(1))
        case Right => curriedApply(pegPosition + new Position(1))
        case _ => throw new RuntimeException(s"Invalid direction: $direction")
      }
    }
    
    def validMovesWithRelativeX(gameBoard: Array[Elements], 
                                foundValues: Array[Array[Elements]], 
                                currentX: Int,
                                directionX: Int): Array[Array[Elements]] = {
       val expectedPegX = currentX + directionX
       val expectedHoleX = currentX + directionX * 2
       if (expectedHoleX < 0 || expectedHoleX >= gameBoard.length) {
         foundValues
       }
       else {
         validMovesWithRelativeX(gameBoard,
                                 if (gameBoard(currentX) == Peg &&
                                     gameBoard(expectedPegX) == Peg && 
                                     gameBoard(expectedHoleX) == Hole) {
                                   foundValues :+ applyJumpRelative(gameBoard, 
                                                                   new Position(currentX), 
                                                                   new Position(directionX * 2))
                                 } else {
                                   foundValues
                                 },
                                 currentX + directionX,
                                 directionX)
       }
    }
    
    def validMoves(gameBoard: Array[Elements]): Array[Array[Elements]] = {
      validMovesWithRelativeX(gameBoard, Array.empty[Array[Elements]], 0, 1) ++ 
      validMovesWithRelativeX(gameBoard, Array.empty[Array[Elements]], gameBoard.length - 1, -1)
    }
    
    def searchForMoves(current: GameState,
                       frontier: Queue[GameState] = Queue[GameState](),
                       searched: Set[GameState] = Set[GameState]()): List[Array[Elements]] = {
      if (goalState(current.gameBoard)) {
        var moves = ListBuffer[Array[Elements]]()
        var currentState = current
        while (currentState != null) {
          moves += currentState.gameBoard
          currentState = currentState.parent
        }
        
        moves.reverse.result
      }
      else {
        searched += current
        frontier ++= validMoves(current.gameBoard).map(new GameState(_, current)).filterNot(searched.contains(_))
        
        if (frontier == Nil) {
          println("failed")
          List(current.gameBoard)
        }
        else {
          searchForMoves(frontier.dequeue, frontier, searched)
        }
      }
    }
    
    def searchForMoves(): List[Array[Elements]] = {
      searchForMoves(new GameState(initialBoard))
    }
    
    def goalState(gameBoard: Array[Elements]): Boolean = {
      gameBoard.count(_ == Peg) == 1
    }
}