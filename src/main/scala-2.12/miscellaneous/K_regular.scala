package miscellaneous

/** P 94
  *
  * @since 2017-02-08
  * @author Park Hyo Jun
  */
class K_regular {

  /**
    * @param value 노드 - 노드의 튜플.
    */
  case class Edge(var value: (Int, Int)){
    if(value._1 > value._2) value = value.swap
    override def toString: String = s"[${value._1}-${value._2}]"
  }

  /**
    * @param n 노드의 갯수
    * @param k 각 노드가 가지는 엣지의 갯수
    * @param lst 노드 목록. 노드 목록은 1 to n이 k번 반복된 값.
    * @param edges 그래프의 엣지들
    * @return 정규 그래프
    */
  private def solve(n: Int, k: Int, lst: List[Int], edges: Set[Edge] = Set.empty): Set[Set[Edge]] = lst match {
    case h::t =>
      val result = for{v <- lst.toSet.filter(_ != h) // 중복된 노드를 제거하고 같은 노드를 제외한 다른 노드와 연결
                          newEdge = Edge(h, v)  // 새로운 엣지
                          nLst = t.diff(List(v))  // 남은 노드에서 이번 선택한 노드만을 제외
                          if !edges.contains(newEdge)} yield solve(n, k, nLst, edges + newEdge)
      result.flatten
    case Nil => Set(edges)
  }

  /**
    * n * k가 짝수개일때만 답이 존재하며
    * 엣지 수는 n * k / 2개라는 것을 알 수 있다.
    * @param n 노드 갯수
    * @param k 엣지 갯수
    * @return 만들어질 수 있는 엣지들
    */
  def solution(n: Int, k: Int): Set[Set[Edge]] =
    if(n * k % 2 != 0 || k > n) Set.empty
    else{
      val lst = List.fill(k)(1 to n).flatten
      solve(n, k, lst)
    }


}

object K_regular extends App {
  val c = new K_regular

  var syst = System.currentTimeMillis()
  val r = c.solution(6, 3)
    r foreach println
  println(r.size)
  println(System.currentTimeMillis() - syst)


}
