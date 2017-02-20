package miscellaneous

import scala.annotation.tailrec
import scala.collection.mutable

/** P98.
  * NP문제 중 하나인 노노그램 풀이 문제
  * 왼쪽부터 오른쪽, 위부터 아래의 팁을 이용해
  * 공통된 영역을 모두 칠한 후 그 영역에 포함되는 경우의 수만을 계산함.
  * 하지만 너무 긴 실행시간...
  *
  * @since 2017-02-12
  * @author Park Hyo Jun
  */
class Nonogram {
  type Board = Vector[Vector[Boolean]]
  var baseBoard: Board = _
  implicit def vecTools[A](vec: Vector[A]): VectorTool[A] = new VectorTool(vec)

  case class Frame(board: Board, topToBottom: Vector[Vector[Int]], leftToRight: Vector[Vector[Int]], rowFill: Vector[Int])

  // 벡터중 가장 큰 값의 포지션
  def getGreaterPos(tip: Vector[Vector[Int]]): Int =
    tip.map(x => if(x.nonEmpty) x.length - 1 + x.sum else 0).zipWithIndex.maxBy(_._1)._2

  // 벡터 중 true인 값들의 index
  def getTruePos(vec: Vector[Boolean]): Vector[Int] = vec.zipWithIndex.filter(x => x._1).map(x => x._2)

  // 벡터의 길이와 tip을 입력받아 생길 수 있는 조합들을 만들어 냄
  def getSubSets(length:Int, vec: Vector[Int]): Set[Vector[Boolean]] = {
    // true들의 벡터를 만드는 메소드
    def toBooleanVector(vec: Vector[Int]): Vector[Vector[Boolean]] = vec match {
      case IndexedSeq() => Vector.empty
      case x +: IndexedSeq() => Vector.fill(x)(true) +: Vector.empty
      case h +: t =>
        val hVector = Vector.fill(h)(true) :+ false
        hVector +: toBooleanVector(t)
    }

    // 벡터의 길이, false의 갯수와 true벡터들을 입력받아 조합하는 메소드
    def makeSet(vecLength: Int, falseCount: Int, composeVec: Vector[Vector[Boolean]]): Set[Vector[Vector[Boolean]]] = {
      if(vecLength == 0) Set(Vector(Vector.fill(falseCount)(false)))
      else
        (0 to falseCount).flatMap{i =>
         makeSet(vecLength-1, falseCount-i, composeVec.tail)
           .map(s => Vector.fill(i)(false) +: composeVec.head +: s)
        }.toSet
    }

    val composeVec = toBooleanVector(vec)
    val falseVecCount = length - composeVec.map(_.length).sum
    makeSet(vec.length, falseVecCount, composeVec).map(_.flatten)
  }

  // 벡터들의 셋에서 공통부분을 찾아냄
  def getIntersect(vec: Set[Vector[Boolean]]): Vector[Boolean] =
    vec.foldLeft(Vector.empty[Boolean]){ (total, n) =>
      if(total.isEmpty) n
      else total.zipWithIndex.map(x => n(x._2) & x._1)
    }

  @tailrec
  final def solveRecu(stack: List[Frame], intersectBoard: Board): Board = stack match {
    case Frame(board, topToBottom, leftToRight, rowFill) :: t =>
      printBoard(board)
      if(leftToRight.exists(_.nonEmpty)){
        val colIndex = getGreaterPos(leftToRight)
        val subSet = getSubSets(topToBottom.length, leftToRight(colIndex))
        val newStacks = subSet
            .filter{set =>
              val c = intersectBoard(colIndex).count(x => x)
              val newC = intersectBoard(colIndex).zipWithIndex.map(x => x._1 & set(x._2)).count(x => x)
              c <= newC
            } .map{set =>
                          val newBoard = board < (colIndex, set)
                          val newTip = leftToRight < (colIndex, Vector.empty)
                          val newFill = set.zipWithIndex.filter(_._1).map(_._2)
                          val newRowFill = newFill.foldLeft(rowFill)((r, x) => r < (x, r(x) + 1))
                          Frame(newBoard, topToBottom, newTip, newRowFill)
                        }.toList
        solveRecu(newStacks ::: t, intersectBoard)
      }else if(topToBottom.exists(_.nonEmpty)){
        val rowIndex = getGreaterPos(topToBottom)
        val subSet = getSubSets(leftToRight.length, topToBottom(rowIndex))
        val row = board.map(x => x(rowIndex))
        if(subSet.contains(row)){
          val newTip = topToBottom < (rowIndex, Vector.empty)
          val newFrame = Frame(board, newTip, leftToRight, rowFill)
          solveRecu(newFrame::t, intersectBoard)
        }else solveRecu(t, intersectBoard)
      }else
        board
    case _ => baseBoard
  }

  def solution(topToBottom: Vector[Vector[Int]], leftToRight: Vector[Vector[Int]]) = {
    val (rowLength, colLength) = (topToBottom.length, leftToRight.length)
    baseBoard = Vector.fill(colLength)(Vector.fill(rowLength)(false))
    val firstFrame = Frame(baseBoard, topToBottom, leftToRight, Vector.fill(rowLength)(0))
    val intersectBoard = getIntersectBoard(baseBoard, topToBottom, leftToRight)
    solveRecu(List(firstFrame), intersectBoard)
  }


  def getIntersectBoard(board: Board, topToBottom: Vector[Vector[Int]], leftToRight: Vector[Vector[Int]]) = {
    var nBoard = board
    // left To Right
    for(i <- leftToRight.indices){
      val se = getSubSets(topToBottom.length, leftToRight(i))
      nBoard = nBoard < (i, getIntersect(se))
    }
    for{i <- topToBottom.indices
        se = getSubSets(leftToRight.length, topToBottom(i))
        intersec = getIntersect(se)
        j <- intersec.indices}{
      val value = nBoard(j)(i) | intersec(j)
      nBoard = nBoard < (j, nBoard(j) < (i, value))
    }
    nBoard
  }


  def printBoard(board: Board): Unit = {
    board.foreach { col =>
      col.foreach(if (_) print(" X |") else print("   |"))
      println()
    }
    println()
  }

}

class VectorTool[A](vec: Vector[A]) {
  def <(pos: Int, value: A): Vector[A] = vec.updated(pos, value)
}


object Nonogram extends App {
  val sysT = System.currentTimeMillis()

  val c = new Nonogram
  val leftToRightTip = Vector(Vector(3), Vector(2, 1), Vector(3, 2), Vector(2, 2), Vector(6), Vector(1, 5), Vector(6), Vector(1), Vector(2))
  val topToBottomTip = Vector(Vector(1, 2), Vector(3, 1), Vector(1, 5), Vector(7, 1), Vector(5), Vector(3), Vector(4), Vector(3))

//  val leftToRightTip =
//    Vector(Vector(5), Vector(2,2), Vector(1,2,2,1), Vector(5,1,1,5), Vector(5,1,5), Vector(5,5,3), Vector(5,1,5,2,2), Vector(11,1,1), Vector(1,1,1,2), Vector(1,4,1,2),
//    Vector(2,1,3,2,1), Vector(2,3,2,5), Vector(2,3,6), Vector(11,1,4), Vector(3,3,6), Vector(9,1,1), Vector(7,1,1), Vector(1,3,1,1,1), Vector(4,5,1), Vector(16,1))
//  val topToBottomTip =
//    Vector(Vector(3,2), Vector(4,2,2), Vector(11,2), Vector(6,4,3), Vector(7,4,1), Vector(2,3,1,4,1), Vector(1,1,2,1,3,1), Vector(1,1,2,1,1,3,1), Vector(1,1,1,3,1,3,1), Vector(2,3,3,4,1),
//    Vector(7,8,1), Vector(6,4,3), Vector(11,2), Vector(4,2,2), Vector(3,3,2), Vector(2,2,6), Vector(2,4), Vector(1,6), Vector(2,2,4), Vector(3,9))

  val result = c.solution(topToBottomTip, leftToRightTip)
  c.printBoard(result)

  println(System.currentTimeMillis() - sysT)
}