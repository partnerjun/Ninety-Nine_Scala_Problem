package binarytree

/**
  * @since 2017-01-07
  * @author Park Hyo Jun
  */
object BinaryTrees extends App{

  val root = Node('a',
              Node('b', Node('d'), Node('e')),
              Node('c', End, Node('f', Node('g'), End)))

//  println(Tree.cBalanced(5, "x"))
//  val x = Node('a', Node('b'), Node('c')).isSymmetric
//  println(root.isSymmetric)

  println(Tree.symmetricBalancedTrees(5, "x"))

}
