## Curator

### Overview

Curator is intended to be a project where I can play around with
implementing immutable data structures. It's sort of a counter-point
to [debox](https://github.com/non/debox), where I play around with
mutable data structures.

Currently Curator contains:

 * a generic, immutable trie implementation

### Getting Curator

Curator is not currently published--you'll need to build it yourself
if you want to try it out.

Curator currently builds against Scala 2.10 and Scala 2.11.

### Example Usage

```scala
import curator.Trie

// build from existing items
val items: Iterable[(String, Int)] = ...
val trie1 = Trie.ofStrings(items)

// build by-hand
val start = Trie.empty[String, Int]
val trie2 = items.foldLeft(start) { case (trie, (k, v)) =>
  trie.set(k, v)
}

// use non-string keys too
val bytes: Iterable[(Array[Byte], String)] = ...
val trie3 = Trie(bytes)

// check whether keys are set
val ok: Boolean = trie1.contains("jim")

// look for values in the trie
val result: Option[Int] = trie1.get("betty")

// search using a prefix
val results: Seq[Int] = trie1.search("j")
```

### Future Work

Add more types.

The interfaces and names for Trie need a lot of work. There are
probably many useful methods which are missing, and it's likely some
of the existing methods should be different as well.

Using `IndexedSeq` to represent the key is a bit ugly and
limiting. Neither `Array` nor `String` directly implement
`IndexedSeq`. Since these are the two types I expect to be most
commonly used with `Trie`, this is a bit ugly. Also, it would be nice
to be able to support a more general mechanism (using octets or bytes
out of a 64-bit long value, for example).

Right now there is no way to remove values out of the trie. This
should just be a matter of implementing the right method.

Trie does not currently tie into Scala collections (besides having a
`.toMap` method). This is probably for the best, since synthesizing
keys on demand to be used with an `Iterable[(IndexedSeq[C], V)]`
interface would be a performance disaster. But it's possible there is
some kind of useful compromise available, especially since in many
cases the full key values are present when the trie is being
constructed.

### Contributing

Building this project requires SBT 0.13.0.

After you launch SBT, you can run the following commands:

 * `compile` compile the project
 * `test` run the tests
 * `console` load a scala REPL with curator on the classpath.

Tests are written with [ScalaTest](http://www.scalatest.org/) and use the
excellent [ScalaCheck](https://github.com/rickynils/scalacheck) library for
automated specification-based testing.

### License

Curator is available to you under the MIT license. See the
[COPYING](COPYING) file for details.

### Credits

Curator is maintained by Erik Osheim.

Copyright (c) 2014 Erik Osheim.
