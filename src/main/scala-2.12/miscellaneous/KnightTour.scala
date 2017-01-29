package miscellaneous

/**
  * @since 2017-01-26
  * @author Park Hyo Jun
  *
  *         Knight's Tour 문제.
  *         Stream을 통해 Lazy 연산을 해 볼 수 있음.
  *
  */

class KnightTour {

  type Point = (Int, Int)

  // 나이트가 이동할 수 있는 좌표들
  val knightMovePoint = List((-2, -1), (-1, -2), (1, 2), (2, 1), (1, -2), (2, -1), (-1, 2), (-2, 1))

  /** 포인트가 이동한 경로를 저장해두는 단위로 사용되는 케이스 클래스
    *
    * @param step   현재 단계
    * @param point  현재 위치
    * @param logSet 지금까지 지나갔던 포인트들의 셋 (중복 검사를 위해 사용됨)
    * @param logLst 지금까지 지나간 포인트들의 리스트
    */
  case class Log(point: Point, step: Int = 1, logSet: Set[Point] = Set.empty, logLst: List[Point] = Nil)

  /** 파라미터로 입력받은 포인트로부터 이동할 수 있는 위치들의 리스트를 리턴하는 메소드
    * Warnsdorf's rule을 이용해 갈수있는 곳이 더 적은 곳 부터 가기 시작함
    *
    * @param point 현재 위치
    * @return 갈 수 있는 다음 위치
    */
  def getNextPoint(point: Point, logSet: Set[Point] = Set.empty, n: Int = 8): List[Point] = {

    def pointValidate(point: Point): Boolean = point._1 > 0 && point._1 <= n && point._2 > 0 && point._2 <= n

    def getPureNextPoint(point: Point): List[Point] = knightMovePoint.map(t => (t._1 + point._1, t._2 + point._2))
                                                                    .filter(pointValidate)
                                                                    .filterNot(logSet.contains)

    getPureNextPoint(point).sortWith(getPureNextPoint(_).length < getPureNextPoint(_).length)
  }


  /**
    * 이동해야 할 곳을 스택으로 쌓아올림.
    * 스택의 가장 윗부분으로는 이동할 수 있는 포인트를 얻을 수 있고
    * 그 포인트들은 다시 스택에 쌓이게 됨.
    *
    * @param moveStack 이동 할 수 있는 포인트들
    * @param n         체스판의 크기
    * @return 첫번째 결과 #:: 나머지 스택을 이용한 재귀
    */
  def knightTourRecu(moveStack: List[Log], n: Int = 8): Stream[List[Point]] = moveStack match {
    case Log(point, step, logSet, logLst) :: t =>
      if (step == n * n) logLst #:: knightTourRecu(t, n) // n * n 회 순회한 경우는 답이 됨
      else {
        // 현재 포인트 정보를 이용해 이동할 수 있는 포인트들을 찾아내고
        // 스택의 tail과 합침
        val newMoveStack = getNextPoint(point, logSet, n)
                                .map(p => Log(p, step + 1, logSet + p, p :: logLst))

        knightTourRecu(newMoveStack ::: t, n) // 새로 만들어진 스택을 이용해 재귀
      }
    case _ => Stream.empty
  }

  def run(n: Int): Stream[List[Point]] = {
    knightTourRecu(List(Log((1, 1), 1, Set((1, 1)), List((1, 1)))), n)
  }

}
