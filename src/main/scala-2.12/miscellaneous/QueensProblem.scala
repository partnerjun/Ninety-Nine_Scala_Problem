package miscellaneous

/**
  * @since 2017-01-20
  * @author Park Hyo Jun
  *
  * P90 N - Queens 문제
  * Excepts 리스트에 필터를 적용하거나
  * selectableLst를 Set으로 하는 방법으로
  * 실행 시간을 더 줄일 수 있음
  */
class QueensProblem {

  def run(num: Int) = {
    eightQueensRecusive((1 to num).toList)
  }

  // selectableLst안에서 제외 목록들에 포함되지 않은 값을 선택해서 재귀호출.
  def eightQueensRecusive(selectableLst: List[Int],
                          result: List[Int] = Nil,
                          leftExcepts: List[Int] = Nil,
                          rightExcepts: List[Int] = Nil): List[List[Int]] = {
    if (selectableLst == Nil) List(result)
    else {
      selectableLst.diff(leftExcepts ::: rightExcepts).flatMap { e =>
        val nextLeftExcepts = (e :: leftExcepts).map(_ - 1)
        val nextRightExcepts = (e :: rightExcepts).map(_ + 1)
        eightQueensRecusive(selectableLst.filter(_ != e), e :: result, nextLeftExcepts, nextRightExcepts)
      }
    }
  }

}
