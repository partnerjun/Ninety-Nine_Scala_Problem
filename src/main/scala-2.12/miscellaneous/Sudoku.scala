package miscellaneous


/**
  * P97. 스도쿠 해결 문제
  *
  * @since 2017-02-09
  * @author Park Hyo Jun
  */
import scala.annotation.tailrec

class Sudoku {
  type Board = Vector[Vector[Short]]
  case class Frame(board: Board)
  val problemBoard: Board = Vector(Vector(0, 0, 4, 8, 0, 0, 0, 1, 7),
                                    Vector(6, 7, 0, 9, 0, 0, 0, 0, 0),
                                    Vector(5, 0, 8, 0, 3, 0, 0, 0, 4),
                                    Vector(3, 0, 0, 7, 4, 0, 1, 0, 0),
                                    Vector(0, 6, 9, 0, 0, 0, 7, 8, 0),
                                    Vector(0, 0, 1, 0, 6, 9, 0, 0, 5),
                                    Vector(1, 0, 0, 0, 8, 0, 3, 0, 6),
                                    Vector(0, 0, 0, 0, 0, 6, 0, 9, 1),
                                    Vector(2, 4, 0, 0, 0, 1, 5, 0, 0))


  def getUseableNums(board: Board, row: Int, col: Int): Set[Short] = {
    val rowNums: Set[Short] = board(col).toSet
    val colNums: Set[Short] = board.map(_(row)).toSet
    val groupNums: Set[Short] = {
      val (groupRow, groupCol) = ((row/3)*3,  (col/3)*3)
      for{ i <- groupRow until groupRow + 3
           j <- groupCol until groupCol + 3 } yield board(j)(i)
    }.toSet

    (1 to 9).map(_.toShort).toSet -- rowNums -- colNums -- groupNums
  }

  @tailrec
  final def getNextEmpty(board: Board, row: Int = 0, col: Int = 0): Option[(Int, Int)] = {
    if(col == 9) None
    else if(row == 9) getNextEmpty(board, 0, col + 1)
    else if(board(col)(row) == 0) Some((row, col))
    else getNextEmpty(board, row + 1, col)
  }


  def solution(stack: List[Frame]): Stream[Board] = stack match {
    case Frame(board)::t =>
      getNextEmpty(board) match {
        case Some((row, col)) =>
          val newStacks = getUseableNums(board, row, col).map{n =>
            Frame(board.updated(col, board(col).updated(row, n)))}.toList
          solution(newStacks ::: t)
        case None =>
          board #:: solution(t)
      }

    case Nil => Stream.empty
  }



  def printBoard(board: Board): Unit ={
    board.zipWithIndex.foreach { v =>
      if(v._2 != 0 && v._2 % 3 == 0) println("---------------------------------")
      v._1.zipWithIndex.foreach{x =>
        if(x._2 != 0 && x._2 % 3 == 0) print(" | ")
        if(x._1 == 0) print(" . ") else print(s" ${x._1} ")}
      println
    }
    println("---------------------------------")
  }

}

object Sudoku extends App {
  val c = new Sudoku
  val firstFrame = c.Frame(c.problemBoard)
  val result = c.solution(List(firstFrame))
  result.foreach(c.printBoard)
}