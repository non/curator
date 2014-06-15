package curator

import org.scalacheck.Arbitrary._
import org.scalatest._
import prop._

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary

abstract class TrieCheck[C, K <% IndexedSeq[C]: Arbitrary, V: Arbitrary]
    extends PropSpec with Matchers with GeneratorDrivenPropertyChecks {

  def fromIterable(k: Iterable[(K, V)]): Trie[C, V]

  def zero: V

  property("trie contains keys") {
    forAll { (keys: List[K]) =>
      val t = fromIterable(keys.map(s => (s, zero)))
      keys.foreach { k => t.contains(k) shouldBe true }
    }
  }

  property("trie only contains correct keys") {
    forAll { (keys: Set[K], others: Set[K]) =>
      val t = fromIterable(keys.map(s => (s, zero)))
      keys.foreach { k => t.contains(k) shouldBe true }
      others.foreach { k => t.contains(k) shouldBe keys(k) }
    }
  }

  property("trie maps keys to values") {
    forAll { (m: Map[K, V]) =>
      val t = fromIterable(m)
      m.foreach { case (k, v) => t.get(k) shouldBe Some(v) }
    }
  }

  def prefixes(k: K): Iterable[K]

  property("trie searches by prefix") {
    forAll { (m: Map[K, V]) =>
      val t = fromIterable(m)
      m.foreach { case (k, v) =>
        prefixes(k).foreach { p =>
          val vs = m.filterKeys(_ startsWith p).values
          t.search(p).toSet shouldBe vs.toSet
        }
      }
    }
  }

  property("tries are equal") {
    forAll { (m: Map[K, V]) =>
      val pairs = m.toList
      val m1 = fromIterable(pairs)
      val m2 = fromIterable(pairs.reverse)
      m1 shouldBe m2
    }
  }
}

class StringToIntTrieCheck extends TrieCheck[Char, String, Int] {
  def fromIterable(m: Iterable[(String, Int)]): Trie[Char, Int] =
    Trie.ofStrings(m)

  def zero: Int = 0

  def prefixes(s: String): Iterable[String] =
    (1 to s.length).map(n => s.substring(0, n))
}

class BytesToStringTrieCheck extends TrieCheck[Byte, Vector[Byte], String] {
  def fromIterable(m: Iterable[(Vector[Byte], String)]): Trie[Byte, String] =
    Trie(m)

  def zero: String = ""

  def prefixes(s: Vector[Byte]): Iterable[Vector[Byte]] =
    (1 to s.length).map(n => s.take(n))
}
