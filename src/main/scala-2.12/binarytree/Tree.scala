package binarytree

import scala.annotation.tailrec

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

abstract class Tree[+T] {

  def preorder: List[T] = ???

  def inorder: List[T] = ???

  def layoutBinaryTree(x: Int = 1, y: Int = 1): (Tree[T], Int) = ???

  def atLevel(lv: Int): List[T] = ???

  def getInternalList: List[T] = ???

  def getLeafNodeCount: Int = ???

  def getLeafList: List[T] = ???

  def isSymmetric: Boolean = ???

  def isMirrorOf[R](tree: Tree[R]): Boolean = ???

  def addValue[U >: T <% Ordered[U]](x: U): Tree[U] = ???
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
    *
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
    *
    * @param number 노드 갯수
    * @param value  노드 값
    * @tparam T 노드 타입
    * @return 만들어진 트리
    */
  def symmetricBalancedTrees[T](number: Int, value: T): List[Tree[T]] = {
    cBalanced(number, value).filter(_.isSymmetric)
  }


  /**
    * P59
    * balanced tree들을 만드는 메소드
    * balanced tree는 리프노드간의 높이 차이가 0<=1이어야 한다.
    * 그 말은, 양쪽에 분배된 높이가 같거나 1차이인 트리를 만들면 된다는 것.
    * 이미 이전 문제에서 map 고차함수를 이용해 트리를 만들었다.
    *
    * @param height 노드 높이
    * @param value  노드 값
    * @tparam T 노트 타입
    * @return 만들어진 트리
    */
  def hbalTrees[T](height: Int, value: T): List[Tree[T]] = height match {
    case 0 => List(End)
    case 1 => List(Node(value, End, End))
    case _ =>
      val greaterLst = hbalTrees(height - 1, value)
      val lessLst = hbalTrees(height - 2, value)
      greaterLst.flatMap(x => greaterLst.map(y => Node(value, x, y))) ::: // 양쪽의 차이가 0인 트리
        lessLst.flatMap(x => greaterLst.flatMap(y => List(Node(value, x, y), Node(value, y, x)))) // 양쪽의 차이가 1인 트리
  }


  // P60
  def minHbalNodes(height: Int): Int = {
    def minM(height: Int): Int = height match {
      case n if n <= 2 => n
      case _ => 2 * minM(height - 1) // 왼쪽 노드에 다 붙은 경우 + 오른쪽 노드에 다 붙은 경우
    }

    // 한 쪽이 다른쪽보다 height 1 더 커야 함. 그 경우의 수가 왼쪽/오른쪽으로 두가지.
    2 * (minM(height - 2) * minM(height - 1))
  }

  def maxHbalHeight(nodes: Int): Int = {
    // 루트 노드와 좌우로 나누어진 노드 수 중 큰 갯수의 합
    1 + Math.floor(nodes / 2).toInt
  }

  def hbalTreesWithNodes[T](nodes: Int, value: T): List[Tree[T]] = {
    // 이렇게 구하지 않고 노드 수를 이용해 노드로 만들 수 있는 최소 높이, 최대 높이를 구한 후
    // hBalTrees 메소드를 이용해 트리들을 만든 후 그 안에서 노드 갯수가 일치하는
    // 트리만 얻어내는 방법도 있음
    def hbalTreesWithNodesRecu(nodes: Int, value: T): List[Tree[T]] = nodes match {
      case n if n <= 1 => List(Node(value))
      case x => hbalTreesWithNodesRecu(x - 1, value)
        .flatMap(x => List(Node(value, x, End), Node(value, End, x)))
    }

    nodes - 1 match {
      case 0 => List(Node(value))
      case n if n % 2 == 0 =>
        val trees = hbalTreesWithNodesRecu(n / 2, value)
        trees.flatMap(l => trees.map(r => Node(value, l, r)))
      case n if n % 2 != 0 =>
        val greaterLst = hbalTreesWithNodesRecu(n / 2 + 1, value)
        val lessLst = hbalTreesWithNodesRecu(n / 2, value)
        lessLst.flatMap(l => greaterLst.flatMap(r => List(Node(value, l, r), Node(value, l, r))))
    }
  }

  /** P63
    *
    * @param nodes 만들 노드의 수
    * @param value 만들 노드의 값
    * @tparam T 노드 타입
    * @return 만들어진 트리
    */
  def completeBinaryTree[T](nodes: Int, value: T): Tree[T] = {
    /** 각 노드에 번호를 붙일 때, 하위 노드의 번호는 왼쪽 2n, 오른쪽 2n+1로 볼 수 있다.
      *
      * @param number 노드의번호
      * @return 하위 노드로 이루어진 트리
      */
    def completeBinaryTreeRecu(number: Int): Tree[T] = {
      if (number > nodes) End
      else Node(value, completeBinaryTreeRecu(2 * number), completeBinaryTreeRecu(2 * number + 1))
    }

    completeBinaryTreeRecu(1) // 첫 노드 1번
  }



  /** P67
    * 문자열을 입력받아 트리로 만드는 메소드
    * @param str 입력받은 문자열
    * @return 만들어진 트리
    */
  def fromString(str: String): Tree[Char] = {

    def createNode(str: String, value: Char = '\0',
                   leftNode: Tree[Char] = End,
                   rightNode: Tree[Char] = End): (Tree[Char], String) = str match {

      case r if r.length ==0 || r.charAt(0) == ')' || r.charAt(0) == ',' =>
        // )나 ,로 시작하는 경우 지금까지 모은 정보로 노드를 만들어낸다
        val others = if(r.length > 0) str.substring(1) else ""
        (Node(value, leftNode, rightNode), others)

      case l if l.charAt(0) == '(' => // (로 시작하는 경우 새 하위노드를 만들기 시작함
        val (node, leftOthers) = createNode(str.substring(1))
        (node, leftOthers)

      case x if x.length > 0 => // 기호가 아닌 경우 첫번째 값을 노드의 value로 사용한다.
        val value = x.charAt(0)
        if(x.length > 1 && (x.charAt(1) == ',' || x.charAt(1) == ')')){
          // 하위노드가 없어 기호를 표시하지 않는 경우에는 바로 노드로 만들어냄
          (Node(value, End, End), x.substring(2))
        }else {
          // value를 제외한 문자열로 왼쪽, 오른쪽 노드를 만들어낸다
          val (leftNode, leftOther) = createNode(x.substring(1))
          val (rightNode, rightOther) = createNode(leftOther)
          createNode(rightOther, value, leftNode, rightNode)
        }
    }

    createNode(str)._1
  }


  /** P68 B
    * preorder 리스트와 inorder 리스트를 파라미터로 입력받아
    * 트리를 만들어내는 문제
    *
    * @param preLst preorder 리스트
    * @param inLst inorder 리스트
    * @return 두 리스트를 참고해 만든 트리
    */
  def preInTree(preLst: List[Char], inLst: List[Char]) : Tree[Char] = {

    /**
      * @param preLst preorder 리스트
      * @param inLst inroder 리스트
      * @return 만들어진 노드
      */
    def preInTreeRecu(preLst: List[Char], inLst: List[Char]): Tree[Char] = preLst match {
      // TODO
    }

    preInTreeRecu(preLst, inLst)
  }
}
