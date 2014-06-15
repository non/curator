package curator

import scala.collection.mutable
import scala.reflect.ClassTag

object Trie {
  def empty[C, V]: Trie[C, V] = Edge(Map.empty, None)

  def single[C, V](key: IndexedSeq[C], v: V): Trie[C, V] =
    nested(0, key, v)

  def nested[C, V](i: Int, key: IndexedSeq[C], v: V): Trie[C, V] = {
    def loop(j: Int, child: Trie[C, V]): Trie[C, V] =
      if (j < i) child
      else loop(j - 1, Edge(Map(key(j) -> child), None))
    loop(key.length - 1, Leaf(v))
  }

  def ofStrings[V](m: Iterable[(String, V)]): Trie[Char, V] =
    m.foldLeft(Trie.empty[Char, V]) { case (t, (k, v)) => t.set(k, v) }

  def apply[C, V](m: Iterable[(IndexedSeq[C], V)]): Trie[C, V] =
    m.foldLeft(Trie.empty[C, V]) { case (t, (k, v)) => t.set(k, v) }
}

sealed trait Trie[C, V] {

  type K = IndexedSeq[C]

  def set(key: K, v: V): Trie[C, V] =
    set(0, key, v)

  def get(key: K): Option[V] =
    getByPrefix(0, key) match {
      case None => None
      case Some(Leaf(v)) => Some(v)
      case Some(Edge(_, ov)) => ov
    }

  def contains(key: K): Boolean =
    getByPrefix(0, key) match {
      case None => false
      case Some(Leaf(v)) => true
      case Some(Edge(_, ov)) => ov.isDefined
    }

  def search(prefix: K): Seq[V] =
    getByPrefix(0, prefix) match {
      case Some(node) =>
        val buf = mutable.ArrayBuffer.empty[V]
        node.foreach(buf += _)
        buf
      case None =>
        Nil
    }

  def map[W](f: V => W): Trie[C, W] =
    this match {
      case Leaf(v) =>
        Leaf(f(v))
      case Edge(cs, ov) =>
        Edge(cs.map { case (c, node) => (c, node.map(f)) }, ov.map(f))
    }
  
  def flatMap[W](f: V => Trie[C, W]): Trie[C, W] =
    this match {
      case Leaf(v) =>
        f(v)
      case Edge(cs, ov) =>
        val e2 = Edge(cs.map { case (c, node) => (c, node.flatMap(f)) }, None)
        ov match {
          case None => e2
          case Some(v) => e2 merge f(v)
        }
    }

  def foreach(f: V => Unit): Unit =
    this match {
      case Leaf(v) =>
        f(v)
      case Edge(cs, ov) =>
        ov.foreach(f)
        cs.foreach { case (_, node) => node.foreach(f) }
    }

  def merge(that: Trie[C, V]): Trie[C, V] = {
    def mergeMaps[C, V](m1: Map[C, Trie[C, V]], m2: Map[C, Trie[C, V]]): Map[C, Trie[C, V]] =
      m2.foldLeft(m1) { case (m, (c2, node2)) =>
        m.get(c2) match {
          case None => m.updated(c2, node2)
          case Some(node1) => m.updated(c2, node1 merge node2)
        }
      }

    this match {
      case Leaf(v1) =>
        that match {
          case Edge(cs, None) => Edge(cs, Some(v1))
          case _ => that
        }
      case Edge(cs1, ov1) =>
        that match {
          case Leaf(v2) => Edge(cs1, Some(v2))
          case Edge(cs2, ov2) => Edge(mergeMaps(cs1, cs2), ov2 orElse ov1)
        }
    }
  }

  def getByPrefix(i: Int, prefix: K): Option[Trie[C, V]] =
    if (i >= prefix.length) Some(this) else this match {
      case Leaf(_) =>
        None
      case Edge(cs, ov) =>
        cs.get(prefix(i)) match {
          case None => None
          case Some(node) => node.getByPrefix(i + 1, prefix)
        }
    }

  def setValue(v: V): Trie[C, V] =
    this match {
      case Leaf(v0) => Leaf(v)
      case Edge(cs, _) => Edge(cs, Some(v))
    }

  def set(i: Int, key: K, v: V): Trie[C, V] =
    if (i >= key.length) setValue(v)
    else this match {
      case Leaf(v0) =>
        Trie.nested(i, key, v).setValue(v0)
      case Edge(cs, ov) =>
        val c = key(i)
        val node = cs.get(c) match {
          case None => Trie.nested(i + 1, key, v)
          case Some(node) => node.set(i + 1, key, v)
        }
        Edge(cs.updated(c, node), ov)
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

    def recurse(m: Map[K, V], len: Int, rcs: List[C], node: Trie[C, V]): Map[K, V] =
      node match {
        case Leaf(v) =>
          m.updated(makeKey(len, rcs), v)
        case Edge(cs, ov) =>
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

case class Leaf[C, V](v: V) extends Trie[C, V]

case class Edge[C, V](cs: Map[C, Trie[C, V]], ov: Option[V]) extends Trie[C, V]
