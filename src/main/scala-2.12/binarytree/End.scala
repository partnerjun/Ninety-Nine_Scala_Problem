package binarytree

/**
  * @since 2017-01-11
  * @author Park Hyo Jun
  */
case object End extends Tree[Nothing] {
  override def layoutBinaryTree(x: Int, y: Int): (Tree[Nothing], Int) = (End, x)

  override def atLevel(lv: Int): List[Nothing] = Nil

  override def getInternalList: List[Nothing] = Nil

  override def getLeafNodeCount: Int = 0

  override def getLeafList: List[Nothing] = Nil

  override def isSymmetric: Boolean = true

  override def isMirrorOf[R](tree: Tree[R]): Boolean = tree == End

  override def addValue[U <% Ordered[U]](value: U): Tree[U] = Node(value)

//  override def toString = "."

  // P67
  override def toString = ""

  override def preorder: List[Nothing] = Nil

  override def inorder: List[Nothing] = Nil


}


