package multiwaytree

/**
  * @since 2017-01-17
  * @author Park Hyo Jun
  */
object MultiwayTree extends App {

  implicit def MultiImp(str: String): MultiwayImplicit = new MultiwayImplicit(str)

  val nodeHead = MTree.fromString("afg^^c^bd^e^^^")
//  println(nodeHead)
//  println(nodeHead.internalPathLength())
//  println(nodeHead.postorder)
  println(nodeHead.lispyTree)
}

class MultiwayImplicit(str: String) {
  def internalPathLength = {
    MTree.fromString(str).internalPathLength()
  }
}