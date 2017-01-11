package binarytree

/**
  * @since 2017-01-07
  * @author Park Hyo Jun
  */
object BinaryTrees extends App{

  val root = Node('a',
              Node('b', Node('d'), Node('e')),
              Node('c', End, Node('f', Node('g'), End)))

  println(Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree()._1)


}
