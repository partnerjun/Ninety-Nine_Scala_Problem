package binarytree

/**
  * @since 2017-01-07
  * @author Park Hyo Jun
  */
object BinaryTrees extends App {

  val root = Node('a',
    Node('b', Node('d'), Node('e')),
    Node('c', End, Node('f', Node('g'), End)))

  //    println(Tree.fromString("a(b(d,e),c(,f(g,)))"))
  //  println(root.preorder)

//  val n = Tree.preInTree(List('a', 'b', 'd', 'e', 'c', 'f', 'g'), List('d', 'b', 'e', 'a', 'c', 'g', 'f'))
//  println(n)
  val tree = Tree.fromString("a(b(d,e),c(,f(g,)))")
  val treeDot = tree.toDotstring

  println(Tree.fromDotstring(treeDot))
}
