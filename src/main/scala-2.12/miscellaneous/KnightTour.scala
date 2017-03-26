package miscellaneous

/**
  * @since 2017-01-26
  * @author Park Hyo Jun
  *
  *         Knight's Tour 문제.
  *         Stream을 통해 Lazy 연산을 해 볼 수 있음.
  *
  */

class KnightTour(n: Int = 8) {

  // 나이트의 '위치'를 위한 튜플
  type Point = (Int, Int)

  /**
    * 나이트가 이동한 경로를 저장해두는 단위로 사용되는 케이스 클래스
    *
    * @param point  현재 위치
    * @param step   이동 횟수
    * @param logSet 지금까지 지나갔던 포인트들의 Set (중복 검사를 위해 사용됨)
    * @param logLst 지금까지 지나간 포인트들의 리스트 (이동 경로)
    */
  case class Log(point: Point = (1, 1), step: Int = 1, logSet: Set[Point] = Set.empty, logLst: List[Point] = Nil)

  // 나이트가 이동할 수 있는 위치들. 왼쪽 위부터 시계 방향으로 적음
  val knightMovePoint = //List((1, -2), (2, -1), (2, 1), (1, 2), (-1, 2), (-2, 1), (-2, -1), (-1, -2))
                         List((-2, -1), (-1, -2), (1, 2), (2, 1), (1, -2), (2, -1), (-1, 2), (-2, 1))

  /**
    * 파라미터로 입력받은 포인트로부터 이동할 수 있는 위치들의 리스트를 리턴하는 메소드
    * Warnsdorf's rule을 이용해 갈수있는 곳이 더 적은 곳 부터 가기 시작함
    *
    * @param point 현재 위치
    * @return 갈 수 있는 다음 위치
    */
  def getNextPoint(point: Point, logSet: Set[Point] = Set.empty): List[Point] = {

    /**
      * 위치가 체스판 안에 있는지 체크하는 메소드
      *
      * @param point 위치
      * @return 이동 가능한 위치인지 여부
      */
    def pointValidate(point: Point): Boolean = 0 < point._1 && point._1 <= n &&
                                               0 < point._2 && point._2 <= n

    /**
      * 입력받은 위치로부터 이동 가능한 위치들을 리턴하는 메소드
      *
      * @param point 위치
      * @return 이동 가능한 위치의 리스트
      */
    def getPureNextPoint(point: Point): List[Point] = knightMovePoint.map(t => (t._1 + point._1, t._2 + point._2))
      .filter(e => pointValidate(e) && !logSet.contains(e))

    // Warndorf's rule에 따라 그 다음 이동 가능한 위치의 갯수별로 정렬
    getPureNextPoint(point).sortWith(getPureNextPoint(_).length < getPureNextPoint(_).length)
  }


  /**
    * 이동해야 할 곳을 스택으로 쌓아올림.
    * 스택의 가장 윗부분으로는 이동할 수 있는 포인트를 얻을 수 있고
    * 그 포인트들은 다시 스택에 쌓이게 됨.
    *
    * @param moveStack 이동 할 수 있는 포인트들
    * @return 첫번째 결과 #:: 나머지 스택을 이용한 재귀
    */
  def solve(moveStack: List[Log] = List(Log())): Stream[List[Point]] = moveStack match {
    case Log(point, step, logSet, logLst) :: tailStack =>
      if (step == n * n) logLst #:: solve(tailStack) // n * n 회 순회한 경우는 답
      else {
        // 현재 위치에서 다음 이동할 수 있는 위치로 이동한 경우들을 스택의 나머지와 합침
        val newMoveStack = getNextPoint(point, logSet)
          .map(p => Log(p, step + 1, logSet + p, p :: logLst))
        solve(newMoveStack ::: tailStack) // 새로 만들어진 스택을 이용해 재귀
      }

    case _ => Stream.empty
  }
}
