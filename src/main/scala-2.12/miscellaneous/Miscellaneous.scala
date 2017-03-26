package miscellaneous


/**
  * @since 2017-01-20
  * @author Park Hyo Jun
  */
object Miscellaneous extends App {

  val sysT = System.currentTimeMillis()

  val knightTour = new KnightTour(8)
  def nextPoints(point: (Int, Int)) = knightTour.knightMovePoint.map(x => (x._1 + point._1, x._2 + point._2))
  val tours = knightTour.solve().filter(lst => nextPoints(lst.head).contains((1, 1)))
  println(tours)
  println(System.currentTimeMillis() - sysT)

}


