package miscellaneous

/**
  * @since 2017-01-20
  * @author Park Hyo Jun
  */
object Miscellaneous extends App {
  val sysT = System.currentTimeMillis()

  val kn = new KnightTour
  val r = kn.run(8)


  r.filter(e => kn.getNextPoint(e.head).contains((1, 1))).take(2).foreach(println)
  println(System.currentTimeMillis() - sysT)

}
