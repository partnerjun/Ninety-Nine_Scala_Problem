package miscellaneous

import scala.annotation.tailrec

/**
  * @since 2017-01-20
  * @author Park Hyo Jun
  */
object Miscellaneous extends App {

  val sysT = System.currentTimeMillis()

//  val synt = new SyntaxChecker
//  val result = synt.identifier("Hell-o")
//  println(result)

//  def fib: Stream[BigInt] = {
//    BigInt(0) #:: BigInt(1) #:: fib.zip(fib.tail).map(x => x._1 + x._2)
//  }

  def fib(n: Int): Int = n match {
    case 0 | 1 => 1
    case _ => fib(n-2) + fib(n-1)
  }

  @tailrec
  def sum1(list: List[BigInt], result: List[BigInt] = Nil): List[BigInt] = list match {
    case Nil => result
    case t :: tail =>
      sum1(tail, t :: result)
  }

val lst: List[BigInt] = (1 to 10).map(x => BigInt(x)).toList
  println(sum1(lst))




  println(System.currentTimeMillis() - sysT)

}


