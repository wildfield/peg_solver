package peg

import Rules._
import Directions._

object Main {
  def main(args: Array[String]) {
      println("start")
      println("orig:\n" + Rules.initialBoard.mkString(" "))
//      println("moves:\n" + Rules.validMoves(Rules.initialBoard).map(_.mkString(" ")).mkString("\n"))
      println("moves:\n" + 
              Rules.searchForMoves().map(_.mkString(" ")).mkString("\n"))
   }
}