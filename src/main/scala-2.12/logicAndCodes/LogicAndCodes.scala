package logicAndCodes

/**
  * @since 2017-01-05
  * @author Park Hyo Jun
  */
object LogicAndCodes extends App {

  implicit def ImplicitClass(a: Boolean): LogicAndCodesImplicitClass = new LogicAndCodesImplicitClass(a)


  val h = new HuffmanCode[String]
  println(h.huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5))))
//  println(h.huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16))))




  // P46
  def table2(f: (Boolean, Boolean) => Boolean): Unit = {
    println("A\tB\tResult")
    for {i <- List(true, false)
         j <- List(true, false)} {
        println(s"$i\t$j\t${f(i, j)}")
    }
  }

  // P49
  // JAVA Style?
  def gray(n: Int): List[String] = {
    def grayRecursive(n: Int): List[String] = {
      if(n == -1) Nil
      else List(n.toBinaryString) ::: grayRecursive(n-1)
    }

    def setLength(s: String, l:Int): String = {
      val sb = new StringBuilder
      for(_ <- s.length until l){
        sb.append("0")
      }
      sb.append(s).toString()
    }
    val r = grayRecursive(n)
    r.map(x => setLength(x, r.head.length)).reverse
  }

  // P47
  class LogicAndCodesImplicitClass(a:Boolean) {
    def and(b: Boolean): Boolean = a & b
    def or(b: Boolean): Boolean = a | b
    def xor(b: Boolean): Boolean = a ^ b
    def equ(b: Boolean): Boolean = a == b
  }



}
