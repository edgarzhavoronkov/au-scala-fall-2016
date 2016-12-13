class B {
   val x = "text"
  lazy val y = x.length
}
class A extends B {
  override val x = "text2"
}

new A().y


trait SumMagnet {
  type Result
  def apply(): Result
}

object SumMagnet {
  implicit def intMagnet(list: List[Int]): SumMagnet = {
    new SumMagnet {
      override def apply(): Int = list.sum

      override type Result = Int
    }
  }

  implicit def stringMagnet(list: List[String]): SumMagnet = {
    new SumMagnet {
      override def apply(): String = list.foldLeft("")(_ + _)

      override type Result = String
    }
  }

  implicit def tupleMagnet(t: (Int, Int)): SumMagnet = {
    new SumMagnet {
      override def apply(): Int = {
        val (a, b) = t
        a + b
      }

      override type Result = Int
    }
  }


}

def sum(sumMagnet: SumMagnet): sumMagnet.Result = {
  sumMagnet()
}

sum(List(1, 2, 3))
sum(List("1", "2", "3"))
sum(1, 2)

implicit class IntExt(i: Int) {
  def twice(): Int = i + i
}

abstract class Graph {
  type Node <: NodeImpl

  abstract class NodeImpl {
    def connect(other: Node)
  }

  def isConnected(n1: Node, n2: Node)

  def addNode: Node
}


val g: Graph = ???
val g2: Graph = ???

val node1 = g.addNode
val node2 = g.addNode

g2.isConnected(node1, node2)








