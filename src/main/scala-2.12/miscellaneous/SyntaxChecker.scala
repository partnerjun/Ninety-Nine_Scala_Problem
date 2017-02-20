package miscellaneous

import scala.util.control.TailCalls._

/** P96
  * 문자나 숫자 사이에만 -이 들어가도록 체크하는 문제
  * 트램폴린 연산을 이용해 최적화해봤다.
  *
  * @since 2017-02-09
  * @author Park Hyo Jun
  */
class SyntaxChecker {

  /** 입력받은 문자열에 -이 마지막에 나오거나 연속으로 나오는지,
    * 캐릭터 혹은 숫자를 확인하는 메소드
    * @param str 문자열
    * @return identifier 통과 여부
    */
  def identifier(str: String): Boolean = {

    def checkerRec(strLst: List[Char], prevChar: Option[Char] = None): TailRec[Boolean] = (strLst, prevChar) match {
      case (h::t, None) => checkerChar(t, Some(h))
      case (l@h::t, Some(c)) =>
        if(c == '-' && (l.isEmpty || h == '-')) done(false)
        else checkerChar(t, Some(h))
      case (Nil, _) => done(true)
    }

    /** 이전 문자가 문자인지 숫자인지 판별하는 메소드
      * @param strLst 캐릭터 리스트
      * @param prevChar 이전 문자
      * @return
      */
    def checkerChar(strLst: List[Char], prevChar: Option[Char]): TailRec[Boolean] = prevChar match {
      case Some(h) if h == '-' => checkerRec(strLst, prevChar)
      case Some(c) if c.isDigit || c.isLetter => checkerRec(strLst, prevChar)
      case _ => done(false)
    }

    checkerRec(str.toList).result
  }
}
