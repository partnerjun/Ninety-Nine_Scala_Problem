package miscellaneous

/**
  * @since 2017-02-02
  * @author Park Hyo Jun
  */
class Von_Koch {

  /** 노드
    * @param name   노드의 이름
    * @param parent 연결된 부모노드
    * @param child  연결된 자식노드
    */
  case class Node(name: String, parent: Option[Node], child: () => List[Node]) {
    override def toString: String = name
  }

  // 문제 예시
  // val nodeA: Node = Node("A", None, () => List(nodeD, nodeG, nodeB))
  // val nodeD: Node = Node("D", Some(nodeA), () => Nil)
  // val nodeG: Node = Node("G", Some(nodeA), () => Nil)
  // val nodeB: Node = Node("B", Some(nodeA), () => List(nodeE, nodeC))
  // val nodeE: Node = Node("E", Some(nodeB), () => List(nodeF))
  // val nodeF: Node = Node("F", Some(nodeF), () => Nil)
  // val nodeC: Node = Node("C", Some(nodeB), () => Nil)
  //
  // val firstStack = {
  //   val nodes = (1 to 7).toList
  //   val edges = (6 to 1 by -1).toList
  //   nodes.map(v => Log(nodeA, nodes.filterNot(_ == v), edges, Map(nodeA -> v)))
  // }


  // 문제 그래프 선언
  val nodeA: Node = Node("A", None, () => List(nodeG, nodeI, nodeH, nodeB, nodeC))
  val nodeG: Node = Node("G", Some(nodeA), () => Nil)
  val nodeI: Node = Node("I", Some(nodeA), () => Nil)
  val nodeH: Node = Node("H", Some(nodeA), () => Nil)
  val nodeB: Node = Node("B", Some(nodeA), () => Nil)
  val nodeC: Node = Node("C", Some(nodeA), () => List(nodeD, nodeF, nodeE))
  val nodeF: Node = Node("F", Some(nodeC), () => Nil)
  val nodeD: Node = Node("D", Some(nodeC), () => List(nodeK))
  val nodeK: Node = Node("K", Some(nodeD), () => Nil)
  val nodeE: Node = Node("E", Some(nodeC), () => List(nodeQ))
  val nodeQ: Node = Node("Q", Some(nodeE), () => List(nodeM, nodeN))
  val nodeM: Node = Node("M", Some(nodeQ), () => Nil)
  val nodeN: Node = Node("N", Some(nodeQ), () => List(nodeP))
  val nodeP: Node = Node("P", Some(nodeN), () => Nil)

  val firstStack: List[Log] = {
    val nodes = (1 to 14).toList
    val edges = (13 to 1 by -1).toList
    nodes.map(v => Log(nodeA, nodes.filterNot(_ == v), edges, Map(nodeA -> v)))
  }

  /** 스트림에 쌓이게 되는 스택
    *
    * @param node      현재 노드
    * @param nodeValue 노드에 할당 가능한 값들
    * @param edgeValue 엣지에 할당 가능한 값들
    * @param logMap    지금까지 노드에 할당한 값들
    */
  case class Log(node: Node, nodeValue: List[Int], edgeValue: List[Int], logMap: Map[Node, Int])

  /**
    * 다음 노드에 할당 가능한 값을 구하는 메소드
    * 엣지 값을 선택하면 현재 노드의 값에 엣지 값을 더하거나 뺀 값이
    * 다음 노드에 할당 가능하다는 점을 이용함.
    *
    * @param value      현재 노드의 값
    * @param nodeValues 노드에 할당 가능한 값들
    * @param edgeValues 엣지에 할당 가능한 값들
    * @return (엣지에 할당한 값, 다음 노드에 할당 가능한 값)의 리스트
    */
  private def nextNodeValues(value: Int, nodeValues: List[Int], edgeValues: List[Int]): List[(Int, List[Int])] =
    edgeValues.map{ edgeValue =>
      (edgeValue, nodeValues.intersect(List(value + edgeValue, value - edgeValue))) }

  /**
    * 다음 노드에 할당 가능한 값들을 찾아 다음 노드의 값을 할당하고
    * 그 노드로 이동한 Log들을 만들어내는 메소드
    *
    * @param nextNode      다음 노드
    * @param thisNodeValue 현재 노드의 값
    * @param nodeValue     노드에 할당 가능한 값
    * @param edgeValue     엣지에 할당 가능한 값
    * @param logMap        지금까지 노드에 할당한 값들
    * @return 새로운 Log의 리스트
    */
  private def getNewStack(nextNode: Node, thisNodeValue: Int, nodeValue: List[Int],
                          edgeValue: List[Int], logMap: Map[Node, Int]): List[Log] =
    for {nextValue     <- nextNodeValues(thisNodeValue, nodeValue, edgeValue)
         nextNodeValue <- nextValue._2} // 다음 노드에 할당 가능한 값
      yield Log(nextNode,
                nodeValue.filterNot(_ == nextNodeValue),
                edgeValue.filterNot(_ == nextValue._1),
                logMap + (nextNode -> nextNodeValue))
    // 할당한 노드, 엣지 값을 제외한 가능한 값들, 할당한 값을 추가한 맵으로 이루어진
    // 새로운 Log들을 만든다.


  /**
    * @param stack 경우의 수를 쌓은 스택
    * @return Map[Node, Int] 형태의 답
    */
  def solve(stack: List[Log] = firstStack): Stream[Map[Node, Int]] = stack match {
    case Log(node, nodeValue, edgeValue, logMap) :: t =>
      if (nodeValue.isEmpty && edgeValue.isEmpty) logMap #:: solve(t) // 모든 값을 할당한 경우 - 답
      else {
        val unvisitedChild = node.child().sortWith(_.child().size < _.child().size)
                                         .filterNot(logMap.keySet.contains)
        if (unvisitedChild.isEmpty) // 방문할 자식노드가 없으면 부모노드로 돌아감
          solve(Log(node.parent.get, nodeValue, edgeValue, logMap) :: t)
        else // 방문할 자식노드가 있으면 그 노드로 이동
          solve(getNewStack(unvisitedChild.head, logMap(node), nodeValue, edgeValue, logMap) ::: t)
      }
    case Nil => Stream.empty
  }

}

object Von_Koch extends App {
  val c = new Von_Koch
  val cur = System.currentTimeMillis()
  c.solve().take(10).foreach(println)
  println(System.currentTimeMillis() - cur)
}
