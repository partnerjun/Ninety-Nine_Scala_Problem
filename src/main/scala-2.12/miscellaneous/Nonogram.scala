package miscellaneous

import scala.annotation.tailrec

/**
  * @since 2017-03-25
  * @author Park Hyo Jun
  */

object NonogramEnum extends Enumeration {
  val O, X, U = Value
}

class Nonogram (rowHints: List[List[Int]], colHints: List[List[Int]]) {
  type Board = List[List[Value]]
  type Value = NonogramEnum.Value
  val ROW_LENGTH: Int = colHints.size
  val COL_LENGTH: Int = rowHints.size
  val rowHintWithIndex: List[(List[Int], Int)] = rowHints.zipWithIndex
  val colHintWithIndex: List[(List[Int], Int)] = colHints.zipWithIndex
  val board: Board = List.fill(COL_LENGTH)(List.fill(ROW_LENGTH)(NonogramEnum.U))

  /**
    * 전체 길이와 힌트를 입력받아
    * 가능한 경우의 수를 모두 만들어내는 메소드
    *
    * @param length 전체 길이
    * @param hint   힌트
    * @return 만들어질 수 있는 모든 경우의 수
    */
  def getAllCases(length: Int, hint: List[Int]): List[List[Value]] = {

    /**
      * 힌트를 입력받아 칠할 부분들을 구하는 메소드
      * 마지막 힌트가 아니면 최소 한 칸의 칠해지면 안되는 부분이 필요하다.
      *
      * @param hint Int 형태의 힌트
      * @return 칠해진 부분들의 그룹
      */
    def hintToGroup(hint: List[Int]): List[List[Value]] = hint match {
      case h :: Nil => List(List.fill(h)(NonogramEnum.O))
      case h :: t =>
        (List.fill(h)(NonogramEnum.O) :+ NonogramEnum.X) :: hintToGroup(t)
    }

    if (hint.isEmpty) List(List.fill(length)(NonogramEnum.X)) // 힌트가 더 이상 없으면 남은 길이만큼 채움
    else {
      val hintGroup = hintToGroup(hint)
      val maxLength = length - hintGroup.map(_.size).sum

      // 0부터 만들 수 있는 최대 갯수만큼의 왼쪽 빈 칸을 만들고
      // 힌트 그룹 하나를 합친 후 나머지 갯수와 힌트로 재귀호출
      (0 to maxLength).flatMap { v =>
        val left = List.fill(v)(NonogramEnum.X) ::: hintGroup.head
        getAllCases(length - left.size, hint.tail).map(left ::: _)
      }.toList
    }
  }

  /**
    * 이미 어느정도 칠해진 부분과 경우의 수를 이용해
    * `무조건 칠해지는 부분`과 `칠해지지 않는 부분`을 구하는 메소드
    *
    * @param nowFilledLine 이미 어느정도 칠해진 부분
    * @param cases      경우의 수들
    * @return 이미 칠한 부분과 경우의 수를 이용해 구한 `무조건 칠해지는 부분`과 `칠해지지 않는 부분`
    */
  def getNextFilledLine(nowFilledLine: List[Value],
                        cases: List[List[Value]]): List[Value] = {

    /**
      * 경우의 수들을 합쳐 공통된 부분을 구하는 함수
      *
      * @param cases 경우의 수들
      * @return 경우의 수들의 공통된 부분
      */
    def foldCases(cases: List[List[Value]]): List[Value] =
      cases.foldLeft(List.empty[Value]) { (total, caseLst) =>
        if (total.isEmpty) caseLst
        else caseLst.zipWithIndex.map(c =>
          if (total(c._2) == c._1) c._1
          else NonogramEnum.U
        )
      }

    /**
      * that이 lst와 `완전히 어긋난` 리스트인지 체크하는 메소드.
      * 연속된 칠 중 일부 부분만 빈 경우를 알아낼 수 있음.
      *
      * @param lst  대상이 되는 리스트
      * @param that 비교할 리스트
      * @return 완전히 포함하는 리스트인지 여부
      */
    @tailrec
    def isContrayCase(lst: List[Value],
                      that: List[Value]): Boolean = (lst, that) match {
      case (Nil, Nil) => false
      case (lstH :: lstT, thatH :: thatT) =>
        if (lstH == NonogramEnum.X && thatH == NonogramEnum.O) true
        else if(lstH == NonogramEnum.O && thatH == NonogramEnum.X) true
        else isContrayCase(lstT, thatT)
    }

    val intersect = cases.filterNot(isContrayCase(nowFilledLine, _))
    if (intersect.size == 1) intersect.head
    else foldCases(intersect)
  }


  /**
    *  행이 칠해진 보드를 리턴하는 메소드
    *
    * @param board 칠하려고 하는 보드
    * @param col 칠하려고 하는 열의 인덱스
    * @param valueLst 칠하려고 하는 값들
    * @return (칠해진 보드, 칠해진 내역)
    */
  def updateRow(board: Board, col: Int, valueLst:List[Value]): (Board, List[Value]) = {
    def getUpdatedLst(lst:List[Value], valueLst: List[Value]): List[Value] = (lst, valueLst) match {
      case (l, Nil) => l
      case (lstH::lstT, valueH::valueT) =>
        val updatedValue = if(lstH == NonogramEnum.U) valueH else lstH
        updatedValue :: getUpdatedLst(lstT, valueT)
    }
    val updatedLst = getUpdatedLst(board(col), valueLst)
    (board.updated(col, updatedLst), updatedLst)
  }

  /**
    *  행이 완성되었을 때 칠해진 부분만 남기는 메소드
    *
    * @param board 보드
    * @param col 완성된 행의 열 인덱스
    * @return 칠해진 부분만 남은 보드
    */
  def closeRow(board: Board, col: Int): Board = {
    val lst = board(col).map(v => if(v != NonogramEnum.O) NonogramEnum.X else v)
    board.updated(col, lst)
  }

  /**
    * 열이 칠해진 보드를 리턴하는 메소드
    *
    * @param board 칠하려고 하는 보드
    * @param row 칠하려고 하는 행의 인덱스
    * @param valueLst 칠하려고 하는 값들
    * @return (칠해진 보드, 칠해진 내역)
    */
  def updateCol(board: Board, row: Int, valueLst: List[Value]): (Board, List[Value]) = {
    def updateColRec(board: Board, row: Int, valueLst: List[Value], col: Int): (Board, List[Value]) = valueLst match {
      case Nil => (board, Nil)
      case valueH :: valueT =>
        val boardValue = board(col)(row)
        val updatedValue = if (boardValue == NonogramEnum.U) valueH else boardValue
        val filledBoard = board.updated(col, board(col).updated(row, updatedValue))
        val (newBoard, updatedLst) = updateColRec(filledBoard, row, valueT, col + 1)
        (newBoard, updatedValue :: updatedLst)
    }
    updateColRec(board, row, valueLst, 0)
  }

  /**
    *  열이 완성되었을 때 칠해진 부분만 남기는 메소드
    *
    * @param board 보드
    * @param col 완성된 열의 행 인덱스
    * @return 칠해진 부분만 남은 보드
    */
  def closeCol(board: Board, col: Int): Board = {
    @tailrec
    def closeColRec(board: Board, row: Int, col: Int): Board = {
      if(col >= COL_LENGTH) return board
      val v = if(board(col)(row) != NonogramEnum.O) NonogramEnum.X else NonogramEnum.O
      val nextBoard = board.updated(col, board(col).updated(row, v))
      closeColRec(nextBoard, row, col + 1)
    }
    closeColRec(board, col, 0)
  }



  /**
    * 리스트가 힌트를 모두 충족한 완성된 리스트인지 체크하는 메소드
    *
    * @param lst  대상 리스트
    * @param hint 힌트
    * @return 완성된 리스트인지 여부
    */
  private def vaildate(lst: List[Value], hint: List[Int]): Boolean =
    lst.count(_ == NonogramEnum.O) == hint.sum


  /**
    * 문제를 풀어내는 메소드
    *
    * @param board 답을 입력할 보드
    * @param rowHint 행을 구할 수 있게 해주는 힌트
    * @param colHint 열을 구할 수 있게 해주는 힌트
    * @param isRow 현재 체크하는 부분이 행인지 여부
    * @param vailCount 행, 혹은 열에서 완성된 줄의 수.
    *                  전체 길이와 같으면 함수가 종료된다.
    *
    * @return 답이 입력된 보드
    */
  @tailrec
  private final def solve(board: Board,
                          rowHint: List[(List[Int], Int)],
                          colHint: List[(List[Int], Int)],
                          isRow: Boolean = true, vailCount: Int = 0): String = {

    // row/column 상황에 맞춰 함수 설정
    val (updateFunction, closeFunction, length, hint) = if(isRow) (updateRow _, closeRow _, ROW_LENGTH, rowHint)
                                                        else (updateCol _, closeCol _, COL_LENGTH, colHint)
    hint match {
      case hintH :: hintT =>
        val (hint, i) = hintH
        val line = if(isRow) board(i) else board.map(_(i))
        val newFill = getNextFilledLine(line, getAllCases(length, hint))

        // 현재에 상황에 맞는 함수로 보드를 채운다.
        val (filledBoard: Board, updatedLst: List[Value]) = updateFunction(board, i, newFill)

        // '완전히 풀어낸 부분' 인지 확인하고 완성된 줄의 수를 체크한다.
        val (nextBoard, nowVailCount) = if(vaildate(updatedLst, hint))
                                          // '완전히 풀어낸 부분'이면 칠해진 부분만 남긴다
                                          (closeFunction(filledBoard, i), vailCount + 1)
                                        else (filledBoard, vailCount)

        solve(nextBoard, hintT, hintT, isRow, nowVailCount)
      case Nil =>
        if (vailCount == length) board
        else solve(board, rowHintWithIndex, colHintWithIndex, !isRow)
    }
  }

  /**
    * 보드를 출력용 문자열로 변환하는 메소드
    *
    * @param board 문자열로 변환할 보드
    * @return 보드로 만든 출력용 문자열
    */
  implicit def boardToString(board: Board): String = {
    val sb = StringBuilder.newBuilder
    board.foreach { col =>
      col.foreach { point =>
        if (point == NonogramEnum.O) sb.append("| O ")
//         else if(point == NonogramEnum.X) sb.append("| X ")
        else sb.append("|   ")
      }
      sb.append("|\n")
    }
    sb.toString()
  }


  /**
    * 결과를 출력하기 위해 호출하는 메소드
    * @return 결과
    */
  def solution: String = solve(board, rowHintWithIndex, colHintWithIndex)
}

object Nonogram extends App {
  // 후크 선장. 20 x 20
  val rowHints = List(
    List(5), List(2, 2), List(1, 2, 2, 1), List(5, 1, 1, 5), List(5, 1, 5),
    List(5, 5, 3), List(5, 1, 5, 2, 2), List(11, 1, 1), List(1, 1, 1, 2), List(1, 4, 1, 2),
    List(2, 1, 3, 2, 1), List(2, 3, 2, 5), List(2, 3, 6), List(11, 1, 4), List(3, 3, 6),
    List(9, 1, 1), List(7, 1, 1), List(1, 3, 1, 1, 1), List(4, 5, 1), List(16, 1)
  )
  val colHints = List(
    List(3, 2), List(4, 2, 2), List(11, 2), List(6, 4, 3), List(7, 4, 1),
    List(2, 3, 1, 4, 1), List(1, 1, 2, 1, 3, 1), List(1, 1, 2, 1, 1, 3, 1), List(1, 1, 1, 3, 1, 3, 1), List(2, 3, 3, 4, 1),
    List(7, 8, 1), List(6, 4, 3), List(11, 2), List(4, 2, 2), List(3, 3, 2),
    List(2, 2, 6), List(2, 4), List(1, 6), List(2, 2, 4), List(3, 9)
  )

  val cur = System.currentTimeMillis()
  val c = new Nonogram(rowHints, colHints)
  println(c.solution)
  println(System.currentTimeMillis() - cur)

}