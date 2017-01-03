package list

/**
  * @since 2017-01-02
  * @author Park Hyo Jun
  */

import scala.annotation.tailrec
import scala.util.Random

object ListProblems extends App {

  val list = List(1, 2, 3, 4, 5)
  println(combinations(2, list))






  // P01
  @tailrec
  def last[A](lst: List[A]): A = lst match {
    case h :: Nil => h
    case _ :: t => last(t)
    case _ => throw new NoSuchElementException
  }

  // P02
  @tailrec
  def penultimate[A](lst: List[A]): A = lst match {
    case h :: _ :: Nil => h
    case _ :: t => penultimate(t)
    case _ => throw new NoSuchElementException
  }

  // P03
  @tailrec
  def nth[A](c: Int, lst: List[A]): A = (c, lst) match {
    case (n, _ :: Nil) if n > 0 => throw new NoSuchElementException
    case (0, h :: _) => h
    case (n, _ :: tail) => nth(n - 1, tail)
  }

  // P04
  def length[A](lst: List[A]): Int = {
    @tailrec
    def lengthRecursive(count: Int, lst: List[A]): Int = (count, lst) match {
      case (c, _ :: Nil) => c
      case (c, _ :: tail) => lengthRecursive(c + 1, tail)
      case (_, Nil) => throw new NoSuchElementException
    }

    lengthRecursive(1, lst)
  }

  // P05
  def reverse[A](lst: List[A]): List[A] = {
    //    @tailrec
    //    def reverseRecursive(result: List[A], lst: List[A]): List[A] = lst match {
    //      case h::Nil => h :: result
    //      case h::t => reverseRecursive(h::result, t)
    //    }
    //    reverseRecursive(Nil, lst)

    lst.foldLeft(List[A]())((l, r) => r :: l)
  }

  // P06
  def isPalindrome[A](lst: List[A]): Boolean = {
    lst == lst.reverse
  }

  // P07
  // FlatMap = Map 이후 Faltten
  def flatten(lst: List[_]): List[_] = lst.flatMap {
    case l: List[_] => flatten(l)
    case i => List(i)
  }

  // P08
  def compress[A](lst: List[A]): List[A] = {
    lst.foldRight(List[A]()) { (l, r) =>
      if (r.isEmpty || r.head != l)
        l :: r
      else
        r
    }
  }


  // P09
  def pack(lst: List[Any]): List[List[Any]] = {
    if (lst.isEmpty) Nil
    else {
      val (packed, others) = lst.span(_ == lst.head)
      packed :: pack(others)
    }
    //    @tailrec
    //    def packRecursive(packed: List[Any], lst: List[A]): List[Any] = lst match {
    //      case Nil => packed
    //      case h::_ =>
    //        val (p, l) = lst.span(_ == h)
    //        packRecursive(p::packed, l)
    //    }
    //    packRecursive(Nil, lst)
  }

  // P10
  def encode[A](lst: List[Any]): List[(Any, Int)] = {
    pack(lst).map(x => (x.head, x.length))
    // wordCount
    // lst.groupBy(x => x).mapValues(_.length).toList
  }

  // P11
  def encodeModified(lst: List[Any]): List[Any] = {
    encode(lst).map { x =>
      if (x._2 == 1) x._1
      else x
    }
  }

  // P12
  def decode[A](lst: List[(Int, A)]): List[Any] = {
    @tailrec
    def decodeRecursive(save: List[A], lst: List[(Int, A)]): List[A] = lst match {
      case h :: Nil => save ++ List.fill(h._1)(h._2)
      case h :: t => decodeRecursive(save ++ List.fill(h._1)(h._2), t)
    }

    decodeRecursive(Nil, lst)
  }


  // P13
  // tailrec시 O(n) reverse나 List:+[A]를 해야 하기 때문.
  def encodeDirect[A](lst: List[A]): List[(Int, A)] = {
    if (lst.isEmpty) Nil
    else {
      val (packed, next) = lst.span(_ == lst.head)
      (packed.length, packed.head) :: encodeDirect(next)
    }
  }

  // P14, 15
  def duplicate[A](n: Int, lst: List[A]): List[A] = {
    //    @tailrec
    //    def duplicateRecursive(c: Int, lst: List[A])(result: List[A] = Nil, originalN: Int = n): List[A] = {
    //      if (lst.isEmpty) result
    //      else if (c == 0) duplicateRecursive(originalN, lst.tail)(result)
    //      else {
    //        duplicateRecursive(c - 1, lst)(lst.head :: result)
    //      }
    //    }
    //
    //    duplicateRecursive(n, lst)().reverse
    lst.flatMap {
      List.fill(n)(_)
    }
  }

  // P16
  def drop[A](n: Int, lst: List[A]): List[A] = {
    lst.zipWithIndex.filterNot(x => (x._2 + 1) == n).map(_._1)
  }

  // P17
  def split[A](n: Int, lst: List[A]): List[Any] = {
    //    val (l, r) = lst.zipWithIndex.partition(_._2 < n)
    //    l.map(_._1) :: r.map(_._1) :: Nil
    lst.take(n) :: lst.drop(n) :: Nil
  }

  // P18
  def slice[A](l: Int, e: Int, lst: List[A]): List[A] = {
    //    lst.drop(l-1).take(e-l) :: Nil
    lst.slice(l, e)
  }

  // P19
  def rotate[A](n: Int, lst: List[A]): List[A] = {
    if (n > 0)
      lst.drop(n) ::: lst.take(n) ::: Nil
    else {
      val length = lst.length - (n * -1)
      lst.drop(length) ::: lst.take(length) ::: Nil
    }
  }

  // P20
  def removeAt[A](n: Int, lst: List[A]): (List[A], A) = {
    val t = lst.splitAt(n)
    (t._1 ::: t._2.tail, t._2.head)
  }

  // P21
  def insertAt(p: Any, n: Int, lst: List[Any]): List[Any] = lst.splitAt(n) match {
    case (pre, post) => pre ::: p :: post
  }

  // P22
  def range(fromValue: Int, toValue: Int): List[Int] = {
    //    (fromValue to toValue).toList
    //    List.range(fromValue, toValue + 1)
    List.iterate(fromValue, toValue - fromValue + 1)(_ + 1)
  }

  // P23
  def randomSelect[A](n: Int, lst: List[A]): List[A] = {
    @tailrec
    def randomSelectRecursive(n: Int, lst: List[A], result: List[A]): List[A] = {
      if (n > 0) {
        val r: Int = (Math.random() * lst.length - 1).toInt
        val removeValue = removeAt(r, lst)
        randomSelectRecursive(n - 1, removeValue._1, removeValue._2 :: result)
      } else
        result
    }

    randomSelectRecursive(n, lst, Nil)
  }

  // P24
  def lotto(n: Int, m: Int): List[Int] = {
    randomSelect(n, List.range(1, m + 1))
  }

  // P25
  def randomPermute(lst: List[Int]): List[Int] = {
    //    randomSelect(lst.length, lst)
    val arr = lst.toArray
    val random = new Random()
    val length = arr.length
    for (_ <- 1 to length) {
      val r1 = random.nextInt(length)
      val r2 = random.nextInt(length)
      val t = arr(r2)
      arr.update(r2, arr(r1))
      arr.update(r1, t)
    }
    arr.toList
  }

  // P26
  def combinations[A](n: Int, lst: List[A]): List[List[A]] = {
    if (n == 0) List(Nil)
    else lst match {
      case Nil => Nil
      case h::t =>
       combinations(n-1, t).map(h::_) :::  combinations(n, t)
    }
  }


}
