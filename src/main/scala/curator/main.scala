package curator

import scala.util.Random.{nextFloat, nextInt, nextGaussian}
import ichi.bench.Thyme

object Main {
  def nextC: Char = (nextInt(26) + 'a').toChar

  def nextKey: String = {
    val arr = new Array[Char](6)
    var i = 0
    while (i < 6) { arr(i) = nextC; i += 1 }
    new String(arr)
  }

  val size = 100000

  val entries = (0 until size).map(_ => (nextKey, nextInt))
  val goodKeys = entries.map(_._1)
  val mostlyBadKeys = (0 until size).map(_ => nextKey)

  def main(args: Array[String]) {
    val th = Thyme.warmedBench(verbose = print)

    println(s"\ntrie: building tree from $size entries")
    val trie = th.pbench { Trie.ofStrings(entries) }

    println(s"\ntrie: successful tree.contains for $size keys")
    th.pbench {
      goodKeys.foldLeft(0)((n, k) => if (trie.contains(k)) n + 1 else n)
    }

    println(s"\ntrie: unsuccessful tree.contains for $size keys")
    th.pbench {
      mostlyBadKeys.foldLeft(0)((n, k) => if (trie.contains(k)) n + 1 else n)
    }

    println(s"\ntrie: successful tree.get for $size keys")
    th.pbench {
      goodKeys.foldLeft(0)((n, k) => if (trie.get(k).isDefined) n + 1 else n)
    }

    println(s"\ntrie: successful tree.search for 10000 keys")
    th.pbench {
      goodKeys.take(10000).map(_.take(4)).foldLeft(0)((n, k) => if (trie.search(k).nonEmpty) n + 1 else n)
    }

    println(s"\ntrie2: building tree from $size entries")
    val trie2 = th.pbench { Trie2.ofStrings(entries) }

    println(s"\ntrie2: successful tree.contains for $size keys")
    th.pbench {
      goodKeys.foldLeft(0)((n, k) => if (trie2.contains(k)) n + 1 else n)
    }

    println(s"\ntrie2: unsuccessful tree.contains for $size keys")
    th.pbench {
      mostlyBadKeys.foldLeft(0)((n, k) => if (trie2.contains(k)) n + 1 else n)
    }

    println(s"\ntrie2: successful tree.get for $size keys")
    th.pbench {
      goodKeys.foldLeft(0)((n, k) => if (trie2.get(k).isDefined) n + 1 else n)
    }

    println(s"\ntrie2: successful tree.search for 10000 keys")
    th.pbench {
      goodKeys.take(10000).map(_.take(4)).foldLeft(0)((n, k) => if (trie2.search(k).nonEmpty) n + 1 else n)
    }
  }
}
