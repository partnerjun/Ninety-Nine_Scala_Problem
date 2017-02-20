package miscellaneous

/** P 94
  *
  * @since 2017-02-08
  * @author Park Hyo Jun
  */
class K_regular {

  case class Edge(var from: Int, var to: Int){
    if(from.compareTo(to) > 0){ // from을 to보다 작은 수로 설정
      val temp = from
      from = to
      to = temp
    }
    override def toString: String = s"[$from-$to]"
  }


  /**
    * @param n 각 노드가 가질 수 있는 엣지의 갯수
    * @param k 노드의 갯수
    * @param lst 노드 목록
    * @param result 결과를 담는 Set
    * @return 노드의 갯수와 엣지의 갯수가 올바른 정규 그래프
    */
  def solve(n: Int, k: Int, lst: List[Int], result:Set[Edge]): Set[Set[Edge]] = lst match {
    case h::t =>
        val set = lst.toSet // Set으로 바꿈으로써 중복된 엘리먼트를 제거
        set.filter(_ != h).flatMap{ v =>
          val newEdge = Edge(h, v)
          val nLst = t.diff(List(v))
          solve(n, k, nLst, result + newEdge)
        }
    case Nil => if(result.size == n * k / 2) Set(result) else Set.empty
  }


  /**
    * n * k가 짝수개일때만 답이 존재하며
    * 엣지 수는 n * k / 2개라는 것을 알 수 있다.
    * @param n 노드 갯수
    * @param k 엣지 갯수
    * @return 만들어질 수 있는 엣지들
    */
  def solution(n: Int, k: Int): Set[Set[Edge]] =
    if(n * k % 2 != 0) Set.empty
    else{
      val lst2 = List.fill(k)(1 to n).flatten
      solve(n, k, lst2, Set.empty)
    }


}
