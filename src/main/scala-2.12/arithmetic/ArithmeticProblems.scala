package arithmetic

import scala.annotation.tailrec

/**
  * @since 2017-01-04
  * @author Park Hyo Jun
  */
object ArithmeticProblems extends App{

  implicit def ImplicitClass(i: Int): Arithmetic = new Arithmetic(i)
//  println(11 isPrime)

  println(100 primeFactors)



  // P32
  // 유클리드 호제법을 이용한 최대공약수
  @tailrec
  def gcd(n: Int, m: Int): Int = {
    // 더 간단한 방법?
    // if (m == 0) m else gcd(m, n % m)

    if (n < m) gcd(m, n)
    else n % m match {
      case 0 => m
      case x => gcd(m, x)
    }
  }

  class Arithmetic(n: Int) {
    // P31
    def isPrime: Boolean = {
      // 2부터 시작해 n까지, 2씩 증가시킨 값으로 n을 나눴을 때 0이 있다면 소수가 아님
      !Stream.range(2, n, 2).exists(n % _ == 0)
    }

    // P33
    def isCoprimeTo(m: Int): Boolean = {
      gcd(n, m) == 1
    }

    // P34
    // 오일러 피 함수
    // 1부터 n까지, n과 최대공약수가 1 인 것(서로소)들의 갯수
    def toient: Int = {
      Stream.range(1, n).count(gcd(n, _) == 1)
    }

    // P35
    def primeFactors: List[Int] = {
      def getPrimeNumbers(limit: Int): Stream[Int] = {
        Stream.range(2, limit).filter(_ isPrime)
      }

//      def primeFactorRecursive(target: Int, s: Stream[Int]): List[Int] = {
//        s.filter(_ * prim)
//      }


      println(getPrimeNumbers(20).toList)
      Nil
    }
  }

}
