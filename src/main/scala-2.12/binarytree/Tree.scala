package binarytree

/**
  * Tree.
  * Full : Child 갯수가 0개 혹은 2개인 경우
  * Balanced : 리프노드의 Height 차이가 0 혹은 1인 경우
  * Complete : 노드들이 왼쪽부터 순서대로 채워진 경우
  * Perfect : Full + Balanced
  *
  * @since 2017-01-07
  * @author Park Hyo Jun
  */

sealed abstract class Tree[+T] {
  def isSymmetric: Boolean = ???
  def isMirrorOf[R](tree: Tree[R]): Boolean = ???
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U] = ???
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString: String = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  /**
    * P56
    * @return 노드의 synmmetric 여부
    */
  override def isSymmetric: Boolean = {
    left.isMirrorOf(right)
  }
  override def isMirrorOf[R](otherNode: Tree[R]): Boolean = otherNode match {
    case x: Node[T] => x.left.isMirrorOf(right) & x.right.isMirrorOf(left)
    case _ => false
  }

  /**
    * P57
    * @param x 추가할 값
    * @return 노드가 추가된 트리
    */
  override def addValue[U >: T <% Ordered[U]](x: U): Tree[U] = x match {
    case a if a < value => Node(value, left.addValue(x), right)
    case _ => Node(value, left, right.addValue(x))
  }
}

case object End extends Tree[Nothing] {
  override def isSymmetric: Boolean = true
  override def isMirrorOf[R](tree: Tree[R]): Boolean = tree == End
  override def addValue[U <% Ordered[U]](value: U): Tree[U] = Node(value)
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}


object Tree {
  /**
    * P55
    * 균형 이진 트리 만들기
    * 각 리프노드간의 height 차이가 0 또는 1인 경우가
    * 균형(balanced)임
    *
    * @param number 하위 노드의 수
    * @param value  채워넣을 값
    * @tparam A 채워넣을 값의 타입
    * @return 만들어진 하위 트리의 리스트
    */
  def cBalanced[A](number: Int, value: A): List[Tree[A]] = number - 1 match {
    case e if e % 2 == 0 =>
      // 짝수개인 경우 Left / Right의 하위노드들이 같을 것이므로
      // 하나의 리스트로 조합을 구해낼 수 있다.
      val lst = cBalanced(e / 2, value)
      // map에 map을 겹치면 이중 리스트가 되므로 flatMap 사용
      lst.flatMap(r => lst.map(l => Node(value, l, r)))
    case e if e % 2 == 1 =>
      // 홀수개인 경우 Left+1 / Right, Left / Right+1 인 경우 두가지가 생길 수 있다.
      val lessLst = cBalanced(e / 2, value)
      val greaterLst = cBalanced(e / 2 + 1, value)
      lessLst.flatMap(l => greaterLst.flatMap(r => List(Node(value, l, r), Node(value, r, l))))
    case _ => List(End)
  }


  /**
    * P57
    * 위에서 만든 addValue를 이용해 순차적으로 넣도록 함
    * @param lst 바이너리 트리로 만들 리스트
    * @tparam U 타입
    * @return 만들어진 트리
    */
  def fromList[U <% Ordered[U]](lst: List[U]): Tree[U] = {
    lst.foldLeft(End: Tree[U])((l, r) => l.addValue(r))
  }


  /**
    * P58
    * symmetric balanced tree들을 만드는 메소드
    * @param number 노드 갯수
    * @param value 노드 값
    * @tparam T 노드 타입
    * @return 만들어진 트리
    */
  def symmetricBalancedTrees[T](number: Int, value: T): List[Tree[T]] = {
    cBalanced(number, value).filter(_.isSymmetric)
  }


  /**
    * P59
    * balanced tree들을 만드는 메소드
    * @param number 노드 갯수
    * @param value 노드 값
    * @tparam T 노트 타입
    * @return 만들어진 트리
    */
  def hbalTrees[T](number: Int, value: T): List[Tree[T]] = {
    // TODO
    Nil
  }
}