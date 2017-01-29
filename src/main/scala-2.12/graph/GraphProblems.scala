package graph

/**
  * @since 2017-01-19
  * @author Park Hyo Jun
  */
object GraphProblems extends App{
  val gr = Digraph.termLabel(List('k', 'm', 'p', 'q'),
    List(('m', 'q', 7), ('p', 'm', 5), ('p', 'q', 9)))

}
