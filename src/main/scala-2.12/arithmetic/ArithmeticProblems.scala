package arithmetic

import scala.annotation.tailrec

/**
  * @since 2017-01-04
  * @author Park Hyo Jun
  */
object ArithmeticProblems extends App {

  implicit def ImplicitClass(i: Int): Arithmetic = new Arithmetic(i)

  /*
  // P37. P38
  val st = System.currentTimeMillis()
  println("totient")
  println(10090 totient)
  println(s"${System.currentTimeMillis() - st} millis. \n")

  val nst = System.currentTimeMillis()
  println("Improved totient")
  println(10090 improvedTotient)
  println(s"${System.currentTimeMillis() - nst} millis. \n")
*/

  printGoldbachListLimited(9 to 20)
  // printGoldbachListLimited(1 to 2000, 50)




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

  // P39
  // 이미 35번에서 해결됨
  def listPrimesinRange(start: Int, end:Int): List[Int] = {
    Stream.range(start, end + 1).filter(_ isPrime).toList
  }

  // P41
  def printGoldbachListLimited(lst: Seq[Int], start: Int = 2): Unit = {
    for{i <- lst if i % 2 == 0}{
      i.goldbach(start) match {
        case (-1, -1) =>
          case g@_ if g._1 != -1 => println(s"$i = ${g._1} + ${g._2}")
        }
    }
  }




  class Arithmetic(n: Int) {
    // P31
    def isPrime: Boolean = {
      // 2부터 시작해 n까지, 1씩 증가시킨 값으로 n을 나눴을 때 0이 있다면 소수가 아님
      !Stream.range(2, n, 1).exists(n % _ == 0)
    }

    // P33
    def isCoprimeTo(m: Int): Boolean = {
      gcd(n, m) == 1
    }

    // P34
    // 오일러 피 함수
    // 1부터 n까지, n과 최대공약수가 1 인 것(서로소)들의 갯수
    def totient: Int = {
      Stream.range(1, n).count(gcd(n, _) == 1)
    }

    // P35
    // 소인수분해 문제
    // n = 24이고 ps가 소수의 집할일 때
    // n은 [ps 중 하나]와 [ps의 엘리먼트 몇가지]와의 곱으로 나타낼 수 있다.
    // ps를 나누어 0이 되는(소인수분해가 가능한 수) 하나를 선택해
    // a라고 한다면 그 값에 곱해지는 값은 n에서 a를 나눈 값이다.
    def primeFactors: List[Int] = {

      def primeFactorRecursive(n: Int, ps: Stream[Int]): List[Int] = {
        if (n.isPrime) List(n)
        else if (n % ps.head == 0)
          ps.head :: primeFactorRecursive(n / ps.head, ps)
        else
          primeFactorRecursive(n, ps.tail)
      }

      // 2부터 limit까지 소수 스트림 구하는거
      def getPrimeNumbers(limit: Int): Stream[Int] = {
        Stream.range(2, limit).filter(_ isPrime)
      }

      primeFactorRecursive(n, getPrimeNumbers(n))
    }


    // P36
    // 35번 갯수 세는 문제
    // wordcounting.
    def primeFactorMultiplicity: List[(Int, Int)] = {
      val lst = n.primeFactors
//       GroupBy를 이용해 Map으로 모으기
       lst.groupBy(x => x).mapValues(_.length).toList

      // foldLeft를 이용해 mutable.Map에 모으기
      // 맵에 추가되는 튜플의 _2는 String이어야 한다고 에러가(?)
      // lst.foldLeft(scala.collection.mutable.Map.empty[Int, Int]){ (map, i) =>
      //   map += (i -> (map.getOrElse(i, 0) + 1) )
      // }.toList
    }


    // P37, 38
    // Euler's totient를 다른 방식으로 푸는 문제
    // P36의 방법으로 구해낸 후 결과를 [p1, m1]라고 할 때,
    // (p1-1)*p1^(m1-1) * ... 를 이용해 구한다.
    def improvedTotient: Int = {
      import scala.math._

      def itRecursive(m: List[(Int, Int)]): Int = m match {
        case h::Nil => ((h._1 - 1) * pow(h._1, h._2 - 1)).toInt
        case h::t =>
          ((h._1 - 1) * pow(h._1, h._2 - 1)).toInt * itRecursive(t)
      }

      itRecursive(n primeFactorMultiplicity)
    }


    // P40
    def goldbach(start: Int = 2) : (Int, Int) = {
      if(n % 2 != 0) (-1, -1)
      else {
        val comb = listPrimesinRange(start, n-1).combinations(2).map(lst => (lst.head, lst(1)))
        comb.find(x => x._1 + x._2 == n).getOrElse((-1, -1))
      }
    }
  }
}
