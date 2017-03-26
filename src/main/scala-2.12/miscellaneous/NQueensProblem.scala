package miscellaneous

import scala.annotation.tailrec

/**
  * @since 2017-02-27
  */
class NQueensProblem {


  /**
    * @param n 문제에서의 N. N x N 체스판에서의 답을 구한다.
    * @return 문제의 답들이 담긴 리스트
    */
  def solve(n: Int): List[List[Int]] = {

    /** 해당 위치에 놓을 수 있는지 여부를 확인하기 위한 메소드
      * @param value 체크하고자 하는 위치
      * @param log 지금까지 놓은 퀸들
      * @param offset 대각선 값을 계산하기 위해 사용되는 조정치
      * @return 놓을 수 있는지 여부
      */
    @tailrec
    def validate(value:Int, log:List[Int], offset:Int = 1):Boolean = log match {
      case h::t =>
        if(value == h - offset || value == h + offset) false
        else validate(value, t, offset + 1)
      case Nil => true // 더 이상 놓은 퀸의 위치가 없다면 true.
    }

    /** 답을 구하기 위해 재귀호출되는 메소드
      * @param selectable 선택 가능한 수
      * @param log 지금까지 퀸을 놓은 열의 리스트
      * @return N개의 수를 모두 사용해 만든 답
      */
    def solveRec(selectable: List[Int], log: List[Int] = Nil): List[List[Int]] = {
      if(selectable.isEmpty)
        List(log) // N개의 수를 모두 사용했다면 답.
      else selectable.withFilter(validate(_, log)) // 이번에 놓을 수 있는 위치를 구함
                     .flatMap{i => solveRec(selectable.filterNot(_ == i), i::log)}
        // 선택 가능한 수 중 수 하나를 선택 가능한 수에서 제외하고
        // 퀸을 놓은 열의 리스트에 기록한 후 재귀호출
    }

    solveRec((1 to n).toList) // 1부터 N까지의 값으로 답을 구하기 시작함
  }

}

object NQueensProblem extends App {

  val cl = new NQueensProblem

  val c = System.currentTimeMillis()
  val r = cl.solve(12)
//  r foreach println
  println(r.length)
  println(System.currentTimeMillis() -c )


}
