package binarytree

/**
  * @since 2017-01-11
  * @author Park Hyo Jun
  */
object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

/**
  * @since 2017-01-11
  * @author Park Hyo Jun
  */
case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString: String = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  /**
    * P56
    *
    * @return 노드의 synmmetric 여부
    */
  override def isSymmetric: Boolean = {
    left.isMirrorOf(right)
  }

  override def isMirrorOf[R](otherNode: Tree[R]): Boolean = otherNode match {
    case x: Node[T] => x.left.isMirrorOf(right) & x.right.isMirrorOf(left)
    case _ => false
  }

  /** P57
    * @param x 추가할 값
    * @return 노드가 추가된 트리
    */
  override def addValue[U >: T <% Ordered[U]](x: U): Tree[U] = x match {
    case a if a < value => Node(value, left.addValue(x), right)
    case _ => Node(value, left, right.addValue(x))
  }

  /** P61
    * @return 해당 노드의 리프노드 갯수
    */
  override def getLeafNodeCount: Int = (left, right) match {
    case (End, End) => 1 // 하위노드 양쪽이 End인 경우 이 노드는 리프노드임
    case _ => left.getLeafNodeCount + right.getLeafNodeCount
  }


  /** P61A
    * 61번을 확장한 문제, 갯수 대신 리스트를 구함
    *
    * @return 리프노드의 리스트
    */
  override def getLeafList: List[T] = (left, right) match {
    case (End, End) => List(this.value)
    case _ => left.getLeafList ::: right.getLeafList
  }


  /** P62
    *
    * @return internal 노드의 리스트
    */
  override def getInternalList: List[T] = (left, right) match {
    case (End, End) => Nil
    case _ => this.value :: left.getInternalList ::: right.getInternalList
  }

  /** P62A
    *
    * @param lv 가져올 레벨
    * @return 해당 레벨의 노드 값들
    */
  override def atLevel(lv: Int): List[T] = lv match {
    case 1 => List(this.value) // 더 깊이 내려갈 필요는 없음
    case _ => left.atLevel(lv - 1) ::: right.atLevel(lv - 1)
  }

  /** P64
    * 기존 트리를 x, y좌표 값이 지정된 트리로 변경하는 문제
    * @return x,y 좌표값이 지정된 트리
    */
  override def layoutBinaryTree(x: Int = 1, y: Int = 1): (Tree[T], Int) = {
      val (leftNode, thisX) = left.layoutBinaryTree(x, y + 1)
      val (rightNode, nextX) = right.layoutBinaryTree(thisX + 1, y + 1)
      (new PositionedNode(value, leftNode, rightNode, thisX, y), nextX)
  }
}