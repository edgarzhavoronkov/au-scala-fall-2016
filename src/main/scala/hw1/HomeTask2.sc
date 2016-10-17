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
  var size0 = array.length

  def this() = this(16)

  def apply(index: Int): Int = array.apply(index)

  def update(index: Int, element: Int): Unit = array.update(index, element)

  def clear(): Unit = reduceToSize(0)

  def +=(element: Int): IntArrayBuffer = {
    ensureSize(array.length + 1)
    array(array.length) = element
    size0 += 1
    this
  }

  def ++=(elements: IntTraversable): IntArrayBuffer = {
    val n = elements.size
    ensureSize(array.length + n)
    ???
    size0 += n
    this
  }

  def remove(index: Int): Int = {
    val result = apply(index)
    ???
    size0 -= 1
    result
  }

  override def isEmpty: Boolean = size0 != 0

  override def size: Int = size0

  override def contains(element: Int): Boolean = array.contains(element)

  override def head: Int = array.head

  override def tail: IntArrayBuffer = {
    val result = new IntArrayBuffer(0)
    var from = 1
    while (from < size0) {
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
    if (size > size0) {
      var newsize = size0 * 2
      while (size > newsize) {
        newsize *= 2
      }
      val newarray = new Array[Int](newsize)
      Array.copy(array, 0, newarray, 0, size0)
      array = newarray
      size0 = newarray.length
    }
  }

  protected def reduceToSize(newSize: Int): Unit = {
    require(newSize <= size0)
    while (size0 > newSize) {
      size0 -= 1
      array(size0) = null
    }
    ???
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

  def apply(elements: Int*): IntArrayBuffer = ???

  def unapplySeq(buffer: IntArrayBuffer): Option[IntArrayBuffer] = ???
}