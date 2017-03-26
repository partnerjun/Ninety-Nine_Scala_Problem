package miscellaneous

/**
  * @since 2017-03-09
  * @author Park Hyo Jun
  */
class ArithmeticPuzzle {

  /**
    *  함수명과 함수를 위한 케이스 클래스.
    * @param name 함수의 이름. toString 메소드에서 사용한다.
    * @param function 함수
    */
  case class ArithmeticFunction(name: String, function: (Int, Int) => Int) {
    def apply(left: Int, right: Int): Int = function(left, right)
    override def toString: String = name
  }

  /** 기록된 함수로 계산한 결과 케이스 클래스.
    * @param value 계산 결과
    * @param str 지금까지 함수를 이용해 계산한
    */
  case class CalcResult(value: Int, str: String) {
    override def toString: String = str
  }

  // +, -, *, / 각 연산들
  val arithmeticFunctions = List(ArithmeticFunction("+", _ + _),
                                 ArithmeticFunction("-", _ - _),
                                 ArithmeticFunction("*", _ * _),
                                 ArithmeticFunction("/", _ / _))

  /**
    * 양 쪽 값을 입력받아 가능한 계산 결과를 만들어 내는 메소드.
    * @param leftValues 왼쪽 값들
    * @param rightValues 오른쪽 값들
    * @return 왼쪽 값과 오른쪽 값으로 만들 수 있는 계산 결과들
    */
  private def calcResults(leftValues: List[CalcResult],
                          rightValues: List[CalcResult]): List[CalcResult] = (leftValues, rightValues) match {
    case (_, Nil) => Nil // 왼쪽으로 값이 모두 넘어간 경우 재귀호출 하지 않음.
    case (Nil, rv::Nil) =>  // 오른쪽 값 하나만 있는 경우 그 값을 그대로 리턴.
      List(rv)              // 재귀호출 할 때 왼쪽 값을 Nil로 시작하기 때문.
    case (lLst, rLst@rh::rt) =>
      val result = for{lv <- calcResults(Nil, lLst)         // 왼쪽 값으로 재귀호출
                      rv <- calcResults(Nil, rLst)          // 오른쪽 값으로 재귀호출
                      func <- arithmeticFunctions           // 가능한 연산들
                      if rv.value != 0 && func.name != "/"} // 0으로 나누는 경우를 제외함.
                   yield CalcResult(func(lv.value, rv.value), s"( $lv $func $rv )")

      // 현재 단계에서 가능한 결과를 모두 구하고 오른쪽 값을 왼쪽 값으로 재귀호출한다.
      result ::: calcResults(lLst :+ rh, rt)
  }


  /** 답을 구하는 메소드
    * @param lst 숫자 리스트
    * @return 구한 결과들
    */
  def solve(lst: List[Int]): List[String] = {

    /** 왼쪽에 들어가는 숫자들로 만든 값과 오른쪽에 들어가는 숫자들로 만든 값이
      * 일치하는 결과를 리턴하는 메소드.
      *
      * @param leftFormula 왼쪽 식에 들어가는 숫자들
      * @param rightFormula 오른쪽 식에 들어가는 숫자들
      * @return 왼쪽 식과 오른쪽 식이 같은 결과들
      */
    def solveRecu(leftFormula: List[Int], rightFormula: List[Int]): List[String] = (leftFormula, rightFormula) match {
      case (_, Nil) => Nil
      case (lLst, rLst@rh::rt) =>
      val result = for { lv <- calcResults(Nil, lLst.map(v => CalcResult(v, v.toString)))
                         rv <- calcResults(Nil, rLst.map(v => CalcResult(v, v.toString)))
                         if lv.value == rv.value} yield s"${lv.str} = ${rv.str}"
      result ::: solveRecu(lLst :+ rh, rt)
    }
    solveRecu(Nil, lst)
  }



}

object ArithmeticPuzzle extends App {
  val c = new ArithmeticPuzzle
  val r = c.solve(List(2, 3, 5, 7, 11))
  r foreach println
}
