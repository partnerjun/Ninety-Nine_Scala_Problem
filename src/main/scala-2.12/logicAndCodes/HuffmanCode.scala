package logicAndCodes

/**
  *
  * P50.
  * 허프만 코드 알고리즘.
  * Either니 Option이니 쓰면 쓸 수록 오히려 복잡해지는 것 같다.
  *
  * @since 2017-01-06
  * @author Park Hyo Jun
  *
  */

class HuffmanCode[A] {

  type Node = (Either[A, HuffmanNode], Int) // (값 혹은 허프만 코드, weight)
  case class HuffmanNode(left: Option[Node], right: Option[Node] = None)

  /**
    * (값, weight) 리스트를 입력받아 (값, 허프만코드) 리스트를 만들어내는 메소드
    *
    * @param lst (값, weight) 리스트
    * @return List[A, HuffmanCode)]
    */
  def huffman(lst: List[(A, Int)]): List[(A, String)] = {

    /**
      * 허프만 코드에서 사용되는 트리를 만드는 메소드
      *
      * @param lst 노드 리스트
      * @return 허프만 트리의 최상위 노드
      */
    def huffmanTree(lst: List[Node]): Node = {

      /**
        * Huffman 객체의 weight를 계산하는 메소드
        *
        * @param s1 허프만노드 값
        * @param s2 허프만노드 값2
        * @return weight
        */
      def getWeight(s1: Option[Node], s2: Option[Node]): Int = {
        s1.getOrElse((null, 0))._2 + s2.getOrElse((null, 0))._2
      }

      /**
        * 리스트에서 왼쪽 두개를 묶어 허프만노드로 만드는 메소드
        *
        * @param lst 노드 리스트
        * @return 왼쪽 두개가 하프만 노드로 묶인 노드 리스트
        */
      def huffmanRecursive(lst: List[Node]): List[Node] = lst.sortWith(_._2 < _._2) match {
        case Nil | _ :: Nil => lst
        case h :: m :: t => huffmanRecursive( (Right(HuffmanNode(Some(h), Some(m))), getWeight(Some(h), Some(m))) :: t )
      }

      huffmanRecursive(lst).head // 루트노드
    }

    /**
      * 노드를 이용해 허프만 코드를 만들어내는 메소드
      *
      * @param root :Node
      * @param fix  :String = ""
      * @return (A, HuffmanCode) 리스트
      */
    def huffmanCodes(root: Node, fix: String = ""): List[(A, String)] = root match {
      case (Left(x), _) => List((x, fix)) // 값 노드인 경우 (값, 허프만 코드) 리턴
      case (Right(x), _) => // 노드인 경우 좌우 재귀호출
        val l: List[(A, String)] = x.left match {
          case Some(z) => huffmanCodes(z, fix + "0")
          case _ => Nil
        }
        val r: List[(A, String)] = x.right match {
          case Some(y) => huffmanCodes(y, fix + "1")
          case _ => Nil
        }
        l ::: r
    }

    huffmanCodes(huffmanTree(lst.map(x => (Left(x._1), x._2))))
  }

}
