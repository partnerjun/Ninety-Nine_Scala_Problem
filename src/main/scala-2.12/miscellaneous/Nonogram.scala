package miscellaneous

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * @since 2017-03-25
  * @author Park Hyo Jun
  */

object NonogramEnum extends Enumeration {
  val O, X, U = Value
}

class Nonogram (rowHints: List[List[Int]], colHints: List[List[Int]]) {
  type Board = ArrayBuffer[ArrayBuffer[NonogramEnum.Value]]
  val ROW_LENGTH: Int = colHints.size
  val COL_LENGTH: Int = rowHints.size
  val rowHintWithIndex: List[(List[Int], Int)] = rowHints.zipWithIndex
  val colHintWithIndex: List[(List[Int], Int)] = colHints.zipWithIndex
  val board: Board = ArrayBuffer.fill(COL_LENGTH)(ArrayBuffer.fill(ROW_LENGTH)(NonogramEnum.U))

  /**
    * 전체 길이와 힌트를 입력받아
    * 가능한 경우의 수를 모두 만들어내는 메소드
    *
    * @param length 전체 길이
    * @param hint   힌트
    * @return 만들어질 수 있는 모든 경우의 수
    */
  def getAllCases(length: Int, hint: List[Int]): List[List[NonogramEnum.Value]] = {

    /**
      * 힌트를 입력받아 칠할 부분들을 구하는 메소드
      * 마지막 힌트가 아니면 최소 한 칸의 칠해지면 안되는 부분이 필요하다.
      *
      * @param hint Int 형태의 힌트
      * @return 칠해진 부분들의 그룹
      */
    def hintToGroup(hint: List[Int]): List[List[NonogramEnum.Value]] = hint match {
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
  def getNextFilledLine(nowFilledLine: List[NonogramEnum.Value],
                        cases: List[List[NonogramEnum.Value]]): List[NonogramEnum.Value] = {

    /**
      * 경우의 수들을 합쳐 공통된 부분을 구하는 함수
      *
      * @param cases 경우의 수들
      * @return 경우의 수들의 공통된 부분
      */
    def foldCases(cases: List[List[NonogramEnum.Value]]): List[NonogramEnum.Value] =
      cases.foldLeft(List.empty[NonogramEnum.Value]) { (total, caseLst) =>
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
    def isContrayCase(lst: List[NonogramEnum.Value],
                      that: List[NonogramEnum.Value]): Boolean = (lst, that) match {
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
    * 리스트가 힌트를 모두 충족한 완성된 리스트인지 체크하는 메소드
    *
    * @param lst  대상 리스트
    * @param hint 힌트
    * @return 완성된 리스트인지 여부
    */
  private def vaildate(lst: List[NonogramEnum.Value], hint: List[Int]): Boolean =
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

    if (isRow) rowHint match {
      case rh :: rt =>
        val (hint, i) = rh
        val result = getNextFilledLine(board(i).toList, getAllCases(ROW_LENGTH, hint))

        // O 혹은 X로 확정된 지점을 채워넣음.
        result.zipWithIndex.withFilter(v => board(i)(v._2) == NonogramEnum.U)
          .foreach(v => board(i)(v._2) = v._1)

        // '완전히 풀어낸 부분' 인지 확인하고 완성된 줄의 수를 체크한다.
        val nowVailCount = if (vaildate(board(i).toList, hint)) {
          for (v <- 0 until ROW_LENGTH)
            if (board(i)(v) != NonogramEnum.O) board(i)(v) = NonogramEnum.X
          vailCount + 1
        } else vailCount

        solve(board, rt, colHint, isRow = true, nowVailCount)
      case Nil =>
        if (vailCount == COL_LENGTH) board
        else solve(board, rowHintWithIndex, colHintWithIndex, isRow = false)

    } else colHint match {
      case ch :: ct =>
        val (hint, i) = ch
        val result = getNextFilledLine(board.map(_ (i)).toList, getAllCases(COL_LENGTH, hint))
        result.zipWithIndex.withFilter(v => board(v._2)(i) == NonogramEnum.U)
          .foreach(v => board(v._2)(i) = v._1)
        val nowVailCount = if (vaildate(board.map(_ (i)).toList, hint)) {
          for (v <- 0 until COL_LENGTH)
            if (board(v)(i) != NonogramEnum.O) board(v)(i) = NonogramEnum.X
          vailCount + 1
        } else vailCount

        solve(board, rowHint, ct, isRow = false, nowVailCount)
      case Nil =>
        if (vailCount == ROW_LENGTH) board
        else solve(board, rowHintWithIndex, colHintWithIndex)
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
        // else if(row == NonogramEnum.X) sb.append("| X ")
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