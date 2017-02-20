package miscellaneous

/** P95
  * StringBuilder를 사용할 수도 있다.
  * CharSequence인 String의 패턴매칭이 불가능한지 에러가 발생한다.
  * @since 2017-02-08
  * @author Park Hyo Jun
  */
class EnglishNumber {

  val numToEng = Map('1' -> "One",
                      '2' -> "Two",
                      '3' -> "Three",
                      '4' -> "Four",
                      '5' -> "Five",
                      '6' -> "Six",
                      '7' -> "Seven",
                      '8' -> "Eight",
                      '9' -> "Nine",
                      '0' -> "Zero")

  private def solve(numLst: List[Char]): List[String] = numLst match {
    case h::Nil => List(numToEng.getOrElse(h, ""))
    case h::t => numToEng.getOrElse(h, "") :: solve(t)
  }

  def fullWords(num: Int) : String = solve(num.toString.toList).mkString("-")
}
