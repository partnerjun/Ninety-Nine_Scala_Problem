package multiwaytree

/**
  * @since 2017-01-17
  * @author Park Hyo Jun
  */

case class MTree[+T](value: T, children: List[MTree[T]] = Nil) {

  /** P71
    * tupler를 이용해 높이가 포함된 튜플로 만들 수 있으므로
    * _2 -1을 하면 루트 높이와의 차이가 됨
    * @param str 트리 문자열
    * @return 루트부터의 높이 차이의 총 합
    */
  def internalPathLength(str: String = this.toString): Int = {
    val tuplers = MTree.tupler(str)
    tuplers.filter(_._1 != '^').foldLeft(0)((total, t) => total + t._2-1)
  }

  /** P70
    * override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
    * @return 만들어진 문자열
    */
  override def toString: String = value.toString + children.foldLeft("")(_ + _.toString) + "^"

  /** P70C
    *
    * @return 노드의 갯수
    */
  def nodeCount: Int = children match {
    case Nil => 1
    case _ => children.foldLeft(1)((total, c) => total + c.nodeCount)
  }

  /** P72
    * @return 요소들을 postorder로 가지고있는 리스트
    */
  def postorder: List[T] = children match {
    case Nil => List(value)
    case _ => children.flatMap(_.postorder) ::: List(value)
  }

  /** P73
    * @return Lisp-Like tree String
    */
  def lispyTree: String = children.length match {
    case 0 => this.value.toString
    case _ =>
      // 차일드가 있는 경우
      // 차일드별로 하위 노드를 구성한 후 차일드의 하위 노드가 없으면 그냥 문자열,
      // 있으면 괄호로 묶인 문자열로 만들어냄
      val string = this.toString
      val tuples = MTree.tupler(string.substring(1))
      val spans = MTree.spaner(tuples)
      val childs: List[String] = spans.map{x =>
                                  val xf = x.filter(_._1 != '^').map(_._1)
                                  if(xf.length == 1) xf.mkString("")
                                  else xf.mkString("(", " ", ")")
                                  }
      val result = s"(${string.charAt(0)} ${childs.mkString(" ")})"
      result
  }

}



object MTree {
  // case class로 선언시 apply를 선언할 수 없음
  // def apply[T](value: T) = new MTree(value, List())
  // def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)


  /** fromString, internalPathLength에서 사용됨
    *
    * @param str MTREE로 만들기 위한 문자열
    * @return (문자열, 트리 높이)로 이루어진 리스트
    */
  private def tupler(str: String): List[(Char, Int)] = {

    /**
      * @param lst    문자열 리스트
      * @param height 트리 높이.  `^`가 나오면 -1 아니면 +1
      * @return (문자열, 높이)로 이루어진 리스트
      */
    def tuplerRecu(lst: List[Char], height: Int): List[(Char, Int)] = lst match {
      case Nil => Nil
      case h :: t =>
        val tuple = if (h == '^') (h, height - 1) else (h, height + 1)
        tuple :: tuplerRecu(t, tuple._2)
    }
    tuplerRecu(str.toList, 0)
  }


  /**
    * 입력받은 리스트를 높이별로 그룹핑함
    * ex) fg^^c^bd^e^^^ => [fg^^, c^, bd^e^^, ^]
    * 그룹핑된 요소로 만들어진 노드는 하위노드가 됨
    *
    * @param tupleLst (문자열, 높이)로 이루어진 리스트
    * @return 입력받은 리스트를 높이별로
    */
  def spaner(tupleLst: List[(Char, Int)]): List[List[(Char, Int)]] = tupleLst match {
    case Nil => Nil
    case h :: t if h._1 == '^' => spaner(t) // ^로 시작하는 캐릭터는 높이 구분을 위한 것이므로 삭제
    case _ :: Nil => List(tupleLst)
    case lst@h :: _ => // 자신의 높이 -1까지는 자신의 하위 노드가 되므로 span으로 그룹핑할 수 있음.
      val (result, others) = lst.span(_._2 != h._2 - 1)
      result :: spaner(others)
  }

  def fromString(str: String): MTree[Char] = {
    /** 입력받은 리스트의 헤드로 노드를 만들고
      * 테일을 spanner 메소드를 이용해 그룹핑한다.
      * 그룹핑된 리스트를 재귀호출하면 노드가 될것이므로 그것들을 하위노드로 삼는다.
      *
      * @param tupleLst (문자열, 높이)로 이루어진 리스트
      * @return 만들어진 노드
      */
    def collection(tupleLst: List[(Char, Int)]): MTree[Char] = tupleLst match {
      case c :: Nil => MTree(c._1) // 하위 노드가 없는 경우
      case h :: t =>
        val spanLst = spaner(t)
        val childs = spanLst.map(collection)
        MTree(h._1, childs)
    }

    collection(tupler(str))
  }

}