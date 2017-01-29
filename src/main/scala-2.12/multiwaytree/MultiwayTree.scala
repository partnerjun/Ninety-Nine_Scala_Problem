package multiwaytree

import list.ListProblems

/**
  * @since 2017-01-17
  * @author Park Hyo Jun
  */
object MultiwayTree extends App {

  implicit def MultiImp(str: String): MultiwayImplicit = new MultiwayImplicit(str)

//  val nodeHead = MTree.fromString("afg^^c^bd^e^^^")
  val nodeHead = MTree("a", List(MTree("b", List(MTree("c")))))
  println(nodeHead.lispyTree)

}

class MultiwayImplicit(str: String) {
  def internalPathLength = {
    MTree.fromString(str).internalPathLength()
  }
}