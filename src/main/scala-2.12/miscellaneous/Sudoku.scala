package miscellaneous
import scala.annotation.tailrec


/**
  * P97. 스도쿠 해결 문제
  *
  * @since 2017-03-20
  * @author Park Hyo Jun
  */
class Sudoku {
  type Board = Vector[Vector[Short]]
  case class Frame(board: Board, points: List[(Int, Int)] = Nil)
  val BOARD_SIZE = 9
  val BOARD_GROUP_SIZE = 3
  val PROBLEM_BOARD: Board = Vector(Vector(0, 0, 4, 8, 0, 0, 0, 1, 7),
    Vector(6, 7, 0, 9, 0, 0, 0, 0, 0),
    Vector(5, 0, 8, 0, 3, 0, 0, 0, 4),
    Vector(3, 0, 0, 7, 4, 0, 1, 0, 0),
    Vector(0, 6, 9, 0, 0, 0, 7, 8, 0),
    Vector(0, 0, 1, 0, 6, 9, 0, 0, 5),
    Vector(1, 0, 0, 0, 8, 0, 3, 0, 6),
    Vector(0, 0, 0, 0, 0, 6, 0, 9, 1),
    Vector(2, 4, 0, 0, 0, 1, 5, 0, 0))

  // 문제 스도쿠 보드와 방문할 좌표의 순서를 적은 스택 첫 번째 값.
  val firstFrame = List(Frame(PROBLEM_BOARD, getEmptyPoints(PROBLEM_BOARD)))

  /**
    * 체크할 스도쿠 보드의 좌표들을 찾는 첫 번째 방법.
    * (0, 0) 부터 (BS - 1, BS - 1) 순서로 찾는다.
    *
    * @param board 체크할 스도쿠 보드
    * @return 아직 값이 선택되지 않은 보드의 좌표들
    */
  def getSimpleEmptyPoints(board: Board): List[(Int, Int)] = {
    val lst = for{row <- 0 until BOARD_SIZE
                  col <- 0 until BOARD_SIZE
                  if board(col)(row) == 0} yield (row, col)
    lst.toList
  }

  /**
    * 체크 할 스도쿠 보드의 좌표들을 찾는 두 번째 방법.
    * 빈 곳의 좌표와 선택 할 수 있는 숫자의 갯수를 구한 후
    * 대입 할 수 있는 수의 갯수 오름차순으로 정렬한다.
    *
    * @param board 체크할 스도쿠 보드
    * @return 대입 할 수 있는 수의 갯수가 적은 순서로 정렬된,
    *         아직 값이 채워지지 않은 좌표들
    */
  def getEmptyPoints(board: Board): List[(Int, Int)] = {
    val lst = for{row <- 0 until BOARD_SIZE
                  col <- 0 until BOARD_SIZE
                  if board(col)(row) == 0
                  numCount = getNextNum(board, (row, col)).size } yield ((row, col), numCount)

    lst.sortBy(_._2).map(_._1).toList
  }

  /**
    * 보드의 좌표에서 선택 가능한 값의 목록을 얻는 메소드
    *
    * 소숫점을 버리는 Int 타입의 특성을 이용해
    * 그룹의 시작 위치를 구할 수 있다.
    *
    * @param board 체크할 스도쿠 보드
    * @param point 체크할 좌표. (row, col).
    * @return 다음으로 선택 가능한 값들
    */
  private def getNextNum(board: Board, point: (Int, Int)): Set[Short] = {
    val rowNums = board(point._2).toSet
    val colNums = board.map(_ (point._1)).toSet
    val groupNums = {
      val (groupRow, groupCol) = ((point._1 / BOARD_GROUP_SIZE) * BOARD_GROUP_SIZE,
        (point._2 / BOARD_GROUP_SIZE) * BOARD_GROUP_SIZE)
      for {row <- groupRow until groupRow + BOARD_GROUP_SIZE
           col <- groupCol until groupCol + BOARD_GROUP_SIZE} yield board(col)(row)
    }.toSet

    (1 to BOARD_SIZE).map(_.toShort).toSet -- rowNums -- colNums -- groupNums
  }


  /**
    * 스도쿠를 풀기 위한 메소드.
    * 지금까지 적은 보드를 이용해 다음 위치를 선택하고
    * 그 위치에 가능한 값 별로 스택을 만들어 쌓아가며 문제를 해결한다.
    *
    * @param stack 값을 하나씩 대입한 스도쿠 보드 스택
    * @return 찾은 정답이 적힌 스도쿠 보드
    */
  @tailrec
  final def solve(stack: List[Frame] = firstFrame): Option[String] = stack match {
    case Frame(board, points) :: frameTail =>
      if(points.isEmpty) Some(boardToString(board))
      else {
        val (row, col) = points.head
        val newStacks = getNextNum(board, points.head).map { num =>
          Frame(board.updated(col, board(col).updated(row, num)), points.tail)
        }.toList
        solve(newStacks ::: frameTail)
      }
    case Nil => None
  }

  /**
    * 스도쿠 보드를 출력 할 수 있는 문자열로 변환하는 메소드
    *
    * @param board 변환할 보드
    * @return 변환된 보드 문자열
    */
  private def boardToString(board: Board): String = {
    val sb = StringBuilder.newBuilder

    for(col <- 0 until BOARD_SIZE){
      if(col % BOARD_GROUP_SIZE == 0) sb.append("---------------------------------\n")

      for(row <- 0 until BOARD_SIZE){
        if(row != 0 && row % BOARD_GROUP_SIZE == 0) sb.append(" | ")
        if(board(col)(row) == 0) sb.append(" . ") else sb.append(s" ${board(col)(row)} ")
      }

      sb.append("\n")
    }
    sb.append("---------------------------------\n").toString()
  }

}
object Sudoku extends App {
  val c = new Sudoku
  val result = c.solve(c.firstFrame)
  println(result)
}