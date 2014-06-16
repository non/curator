package curator

import scala.collection.mutable
import scala.reflect.ClassTag

object Trie2 {
  def empty[C, V]: Trie2[C, V] = Trie2(Map.empty, None)

  def ofKeys[C, V](m: Iterable[(IndexedSeq[C], V)]): Trie2[C, V] =
    m.foldLeft(Trie2.empty[C, V]) { case (t, (k, v)) => t.set(k, v) }

  def ofStrings[V](m: Iterable[(String, V)]): Trie2[Char, V] =
    m.foldLeft(Trie2.empty[Char, V]) { case (t, (k, v)) => t.set(k, v) }
}

case class Trie2[C, V](cs: Map[C, Trie2[C, V]], ov: Option[V]) {

  type K = IndexedSeq[C]

  def set(key: K, v: V): Trie2[C, V] =
    set(0, key, v)

  def get(key: K): Option[V] =
    getByPrefix(0, key).flatMap(_.ov)

  def contains(key: K): Boolean =
    getByPrefix(0, key).map(_.ov.isDefined).getOrElse(false)

  def search(prefix: K): Seq[V] =
    getByPrefix(0, prefix).map(_.values).getOrElse(Nil)

  def values: Seq[V] =
    if (cs.isEmpty) ov.toList else {
      val buf = mutable.ArrayBuffer.empty[V]
      foreach(buf += _)
      buf
    }

  def map[W](f: V => W): Trie2[C, W] =
    Trie2(cs.map { case (c, node) => (c, node.map(f)) }, ov.map(f))
  
  def flatMap[W](f: V => Trie2[C, W]): Trie2[C, W] = {
    val e2 = Trie2(cs.map { case (c, node) => (c, node.flatMap(f)) }, None)
    ov match {
      case None => e2
      case Some(v) => e2 merge f(v)
    }
  }

  def foreach(f: V => Unit): Unit = {
    ov.foreach(f)
    cs.foreach { case (_, node) => node.foreach(f) }
  }

  def merge(that: Trie2[C, V]): Trie2[C, V] = {
    def mergeMaps[C, V](m1: Map[C, Trie2[C, V]], m2: Map[C, Trie2[C, V]]): Map[C, Trie2[C, V]] =
      m2.foldLeft(m1) { case (m, (c2, node2)) =>
        m.get(c2) match {
          case None => m.updated(c2, node2)
          case Some(node1) => m.updated(c2, node1 merge node2)
        }
      }

    Trie2(mergeMaps(cs, that.cs), that.ov orElse ov)
  }

  def getByPrefix(i: Int, prefix: K): Option[Trie2[C, V]] =
    if (i >= prefix.length) Some(this)
    else cs.get(prefix(i)) match {
      case None => None
      case Some(node) => node.getByPrefix(i + 1, prefix)
    }

  def setValue(v: V): Trie2[C, V] = Trie2(cs, Some(v))

  def set(i: Int, key: K, v: V): Trie2[C, V] = {

    def nested(start: Int): Trie2[C, V] = {
      def loop(j: Int, child: Trie2[C, V]): Trie2[C, V] =
        if (j < start) child
        else loop(j - 1, Trie2(Map(key(j) -> child), None))
      loop(key.length - 1, Trie2(Map.empty, Some(v)))
    }

    if (i >= key.length) setValue(v)
    else {
      val c = key(i)
      val node = cs.get(c) match {
        case None => nested(i + 1)
        case Some(node) => node.set(i + 1, key, v)
      }
      Trie2(cs.updated(c, node), ov)
    }
  }

  def toMap(implicit ct: ClassTag[C]): Map[K, V] = {

    def makeKey(len: Int, rcs: List[C]): K = {
      val arr = new Array[C](len)
      var i = len - 1
      var lst = rcs
      while (i >= 0) {
        arr(i) = lst.head
        lst = lst.tail
        i -= 1
      }
      arr
    }

    def recurse(m: Map[K, V], len: Int, rcs: List[C], node: Trie2[C, V]): Map[K, V] = {
      val m2 = ov match {
        case Some(v) => m.updated(makeKey(len, rcs), v)
        case None => m
      }
      cs.foldLeft(m2) { case (m, (c, node)) =>
        recurse(m, len + 1, c :: rcs, node)
      }
    }

    recurse(Map.empty, 0, Nil, this)
  }
}
