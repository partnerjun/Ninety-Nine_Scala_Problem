package miscellaneous

/** P93
  *
  * @since 2017-02-04
  * @author Park Hyo Jun
  */
class Arithmetic_Puzzle(lst: List[Int]) {

  case class fund(name: String, function: (Int, Int) => Int) {
    def apply(h: Int, t: Int): Int = function(h, t)
    override def toString: String = name
  }

  case class Result(num: Int, str: String) {
    override def toString: String = str
  }

  val plusCalc = fund(" + ", _ + _)
  val minusCalc = fund(" - ", _ - _)
  val multiCalc = fund(" * ", _ * _)
  val divideCalc = fund(" / ", _ / _)
  val calcList: List[fund] = List(plusCalc, minusCalc, multiCalc, divideCalc)



  def solveNum(headLst: List[Result], tailLst: List[Result]): List[Result] = (headLst, tailLst) match {
    case (_, Nil) => Nil
    case (Nil, h::Nil) => List(Result(h.num, s"$h"))
    case (hl, tl@tlh::tlt) =>
      val result = for{h <- solveNum(Nil, hl)
                       t <- solveNum(Nil, tl)
                       f <- calcList // + - * /
                       if t.num != 0 && f != divideCalc // divide by zero
                       } yield Result(f(h.num, t.num), s"($h$f$t)")
      result ::: solveNum(hl:+tlh, tlt)
  }

  def solve(headLst: List[Int], tailLst: List[Int]): List[String] = tailLst match {
    case h::t =>
      val result = for{h <- solveNum(Nil, headLst.map(x => Result(x, x.toString)))
                       t <- solveNum(Nil, tailLst.map(x => Result(x, x.toString)))
                       if h.num == t.num} yield s"$h = $t"
      result ::: solve(headLst:+h, t)
    case _ => Nil
  }



  def answer = {
  val r = solve(Nil, lst)
    r foreach println
  }

}
