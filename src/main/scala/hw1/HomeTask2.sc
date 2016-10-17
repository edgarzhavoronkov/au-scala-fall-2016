//Реалзуйте IntArrayBuffer с интерфейсом IntTraversable
trait IntTraversable {
  def isEmpty: Boolean

  def size: Int

  def contains(element: Int): Boolean

  def head: Int

  def tail: IntTraversable

  def ++(traversable: IntTraversable): IntTraversable

  def filter(predicate: Int => Boolean): IntTraversable

  def map(function: Int => Int): IntTraversable

  def flatMap(function: Int => IntTraversable): IntTraversable

  def foreach(function: Int => Unit): Unit
}

class IntArrayBuffer(initialLength : Int) extends IntTraversable {
  var array = new Array[Int](initialLength)

  def this() = this(16)

  def apply(index: Int): Int = array.apply(index)

  def update(index: Int, element: Int): Unit = array.update(index, element)

  def clear(): Unit = reduceToSize(0)

  def +=(element: Int): IntArrayBuffer = {
    val newarray = new Array[Int](array.length + 1)
    Array.copy(array, 0, newarray, 0, array.length)
    newarray(array.length) = element
    array = newarray
    this
  }

  def ++=(elements: IntTraversable): IntArrayBuffer = {
    for (x <- elements) {
      this += x
    }
    this
  }

  def remove(index: Int): Int = {
    val result = apply(index)
    val newarray = new Array[Int](array.length - 1)
    Array.copy(array, 0, newarray, 0, index)
    Array.copy(array, index + 1, newarray, index, array.length - 1 - index)
    array = newarray
    result
  }

  override def isEmpty: Boolean = array.isEmpty

  override def size: Int = array.length

  override def contains(element: Int): Boolean = array.contains(element)

  override def head: Int = array.head

  override def tail: IntArrayBuffer = {
    val result = new IntArrayBuffer(0)
    var from = 1
    while (from < array.length) {
      result += array(from)
      from += 1
    }
    result
  }

  override def ++(traversable: IntTraversable): IntArrayBuffer = {
    val result = new IntArrayBuffer(0)
    result ++= this
    result ++= traversable
    result
  }

  protected def ensureSize(size: Int): Unit = {
    if (size > array.length) {
      var newsize = array.length * 2
      while (size > newsize) {
        newsize *= 2
      }
      val newarray = new Array[Int](newsize)
      Array.copy(array, 0, newarray, 0, array.length)
      array = newarray
    }
  }

  protected def reduceToSize(newSize: Int): Unit = {
    require(newSize <= array.length)
    array = array.take(newSize)
  }

  override def filter(predicate: (Int) => Boolean): IntTraversable = {
    val result = new IntArrayBuffer(0)
    for (x <- array) {
      if (predicate(x)) {
        result += x
      }
    }
    result
  }

  override def map(function: (Int) => Int): IntTraversable = {
    val result = new IntArrayBuffer(0)
    for (x <- array) {
      result += function.apply(x)
    }
    result
  }

  override def flatMap(function: (Int) => IntTraversable): IntTraversable = {
    val result = new IntArrayBuffer(0)
    for (x <- array) {
      result ++= function.apply(x)
    }
    result
  }

  override def foreach(function: (Int) => Unit): Unit = {
    for (x <- array) {
      function.apply(x)
    }
  }
}

object IntArrayBuffer {
  def empty: IntArrayBuffer = new IntArrayBuffer(0)

  def apply(elements: Int*): IntArrayBuffer = {
    val result = new IntArrayBuffer(0)
    for (elem <- elements) {
      result += elem
    }
    result
  }
//  scalac bug?
  def unapplySeq(buffer: IntArrayBuffer): Option[IntArrayBuffer] = {
    if (buffer.isEmpty) {
      None
    } else {
      Some(buffer)
    }
  }
}

val i = IntArrayBuffer.empty
val j = IntArrayBuffer.empty

val k = IntArrayBuffer(6, 7, 8)


i += 1 += 2 += 3

j += 4 += 5 += 6

i ++= j

for (x <- k) {
  print(x.asInstanceOf[Int] + "\n")
}

//  scalac bug?
k match {case IntArrayBuffer(a) => print(a)}
