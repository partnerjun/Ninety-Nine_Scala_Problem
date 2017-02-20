package miscellaneous

import scala.collection.mutable


/** P 92.
  * 더 많은 경우의 수를 처리하기 위해 지연 콜렉션을 사용
  *
  * knight's tour와 접근 방법은 비슷했지만
  * 신경써야 하는 점이 몇가지 더 있음.
  * knight's tour는 이미 지났던 길로 가지 않지만 이 문제에서는 지나야 함.
  * 또한 엣지와 노드 자체에 값을 부여해야 하기 때문에 그 계산도 필요함.
  *
  * child가 있을 때
  *1. 현재 노드에 값 설정
  *2. 현재 노드의 값을 기준으로 가능한 다음 노드의 값과 엣지의 값을 구함
  *3. 방문하지 않은 child 노드들의 Frame을 만들어 Stack에 쌓음.
  *
  * child가 없을 때
  * 각 결과에 현재 노드에 설정 가능한 값들을 추가한 후
  * 부모 노드에 방문할 프레임으로 변경해서 Stack에 다시 쌓음.
  *
  * 차일드 노드가 적은 순서대로, 더 작은 수를 할당하여 더 빠르게 해결
  *
  * @since 2017-02-01
  * @author Park Hyo Jun
  */
object Von_Koch_conjecture extends App {

  case class Node(name: String, var parent: Option[Node] = None, child: mutable.Set[Node] = mutable.Set.empty) {
    override def hashCode(): Int = name.hashCode

    // Set에 있는 node값을 해싱하려 하면 루프를 돌게 됨
    override def toString: String = s"$name"
  }

  def setup_ex = {
    // 문제 예시 그래프
    val nodeA = Node("a")
    val nodeB = Node("b", Some(nodeA))
    val nodeC = Node("c", Some(nodeB))
    val nodeD = Node("d", Some(nodeA))
    val nodeE = Node("e", Some(nodeB))
    val nodeF = Node("f", Some(nodeE))
    val nodeG = Node("g", Some(nodeA))

    nodeA.child ++= Set(nodeD, nodeB, nodeG)
    nodeB.child ++= Set(nodeE, nodeC)
    nodeE.child += nodeF

    nodeA
  }

  def setup_p2 = {
    // 문제 그래프
    val nodeA = Node("a")
    val nodeB = Node("b", Some(nodeA))
    val nodeC = Node("c", Some(nodeA))
    val nodeD = Node("d", Some(nodeC))
    val nodeE = Node("e", Some(nodeC))
    val nodeF = Node("f", Some(nodeC))
    val nodeG = Node("g", Some(nodeA))
    val nodeH = Node("h", Some(nodeA))
    val nodeI = Node("i", Some(nodeA))
    val nodeK = Node("k", Some(nodeD))
    val nodeQ = Node("q", Some(nodeE))
    val nodeN = Node("n", Some(nodeQ))
    val nodeP = Node("p", Some(nodeN))
    val nodeM = Node("m", Some(nodeQ))

    nodeA.child ++= Set(nodeG, nodeI, nodeH, nodeH, nodeB, nodeC)
    nodeD.child += nodeK
    nodeC.child ++= Set(nodeD, nodeE, nodeF)
    nodeE.child += nodeQ
    nodeQ.child ++= Set(nodeM, nodeN)
    nodeN.child += nodeP

    nodeA
  }


  /** 스택에 쌓게되는 정보
    * @param nowNode 현재 방문 중인 노드
    * @param thisNodeValue 현재 방문 중인 노드에 할당 가능한 값
    * @param nodeValues 전체 노드에 할당 가능한 값
    * @param edgeValues 전체 엣지에 할당 가능한 값
    * @param result 현재까지 방문한 노드와 할당한 값들
    */
  case class Frame(nowNode: Node, thisNodeValue: Set[Int], nodeValues: Set[Int], edgeValues: Set[Int], result: Set[(Node, Int)])

  /**
    * 현재 노드에 선택된 값과 엣지에 선택가능한 값을 이용해
    * 다음 노드의 값을 선택하는 메소드
    *
    * @param thisNodeValue 현재 노드에 선택된 값
    * @param nodeValues    사용가능한 노드들의 값
    * @param edgeValues    사용가능한 엣지들의 값
    * @return 노드의 값, 엣지의 값의 Set
    */
  def getNodeAndEdgeValues(thisNodeValue: Int, nodeValues: Set[Int], edgeValues: Set[Int]): Set[(Set[Int], Int)] =
    edgeValues.map { edgeValue =>
      val sValue = thisNodeValue - edgeValue
      val bValue = thisNodeValue + edgeValue
      // 현재 노드값에 값을 더하거나 뺀 값이 있는 (사용가능한) edgeValues만 리턴
      (nodeValues.intersect(Set(sValue, bValue)), edgeValue)
    }

  /**
    * 다음으로 방문할 노드를 선택하는 메소드
    * 파라미터로 입력받은 노드 중 하위 노드가 적은 순서대로 방문(경우의 수를 줄임)
    *
    * @param nodes 방문 가능한 노드들
    * @return 다음으로 방문할 노드
    */
  def getNextNode(nodes: mutable.Set[Node]): Node = nodes.toList.sortWith(_.child.size < _.child.size).head


  /** 방문했던 노드들에 현재 노드가 포함되어있으면 그 값 리턴
    *
    * @param node   현재 노드
    * @param result 방문했던 노드들
    * @return (현재 노드, 값)
    */
  def getContainValueSet(node: Node, result: Set[(Node, Int)]): Set[Int] = result.filter(_._1 == node).map(_._2)


  /**
    * @param stack Frame의 리스트
    * @return 결과
    */
  def solve(stack: List[Frame]): Stream[Set[(Node, Int)]] = stack match {
    case Frame(node, thisNodeValue, nodeValues, edgeValues, result) :: t =>

      if (edgeValues.isEmpty && nodeValues.isEmpty) result #:: solve(t) // 모든 값을 할당 성공한 경우 -> 답
      else if (thisNodeValue.isEmpty) solve(t) // 이 노드에 사용가능한 값이 없는 경우는 답이 될 수 없으므로 스택 맨 위 작업을 이어함
      else {
        val child = node.child -- result.map(_._1)
        // 이동 가능한 child만을 추려냄
        val containValueSet = getContainValueSet(node, result) // 이미 방문했었던 노드인지 확인

        if (child.isEmpty) { // 이동가능한 child가 없을 때
          if (containValueSet.nonEmpty) {
            // 이미 방문했던 노드는 상위 노드로 변경.
            node.parent match {
              case Some(x) => solve(Frame(x, nodeValues, nodeValues, edgeValues, result) :: t)
              case None => solve(t) // 상위 노드가 없는 경우는 답이 아니므로 스택 맨 위 작업을 이어함
            }
          } else {
            // 처음 방문한 노드인 경우 할당 가능한 값 별로 Frame을 만들어 stack에 쌓음
            val newStacks = thisNodeValue.toList.sorted.map { v =>
              val availNodeValue = nodeValues - v
              Frame(node.parent.get, availNodeValue, availNodeValue, edgeValues, result + ((node, v)))
            }
            solve(newStacks ::: t)
          }

        } else { // 이동가능한 child가 있을 때
          val nextNode = getNextNode(child) // 다음으로 방문할 child Node를 선택
          val newStack =
            if (containValueSet.nonEmpty) {
              val nodeValue = containValueSet.head
              val availNodeAndEdgeValues = getNodeAndEdgeValues(nodeValue, nodeValues, edgeValues)
              availNodeAndEdgeValues.toList.map ( nodeAndEdgeValue =>
                Frame(nextNode, nodeAndEdgeValue._1, nodeValues, edgeValues - nodeAndEdgeValue._2, result)
              )
            } else // 방문하지 않은 노드인 경우
              thisNodeValue.toList.sorted.flatMap { nodeValue => // 현재 노드의 값이 될 수 있는 경우의 수 * 엣지 노드의 값이 될 수 있는 수 만큼 스택
                val availNodeValues = nodeValues - nodeValue
                val availNodeAndEdgeValues = getNodeAndEdgeValues(nodeValue, availNodeValues, edgeValues)
                availNodeAndEdgeValues.map ( nodeAndEdgeValue =>
                  Frame(nextNode, nodeAndEdgeValue._1, availNodeValues, edgeValues - nodeAndEdgeValue._2, result + ((node, nodeValue)))
                  )
              }

          solve(newStack ::: t)
        }
      }
    case Nil => Stream.empty
  }

  // 실행 테스트
  val root = setup_p2
  val nodeSet = (1 to 14).toSet
  val edgeSet = (1 to 13).toSet
  val firstFrame = nodeSet.map(x => Frame(root, nodeSet - x, nodeSet - x, edgeSet, Set((root, x)))).toList
  val sysT = System.currentTimeMillis()

  val result = solve(firstFrame).take(1)
  result foreach println

  println(System.currentTimeMillis() - sysT)
}
