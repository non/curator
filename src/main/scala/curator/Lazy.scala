package curator

import scala.collection.{immutable, mutable}

object Lazy {
  def apply[A](as: Iterable[A]): Lazy[A] =
    new Lazy[A] {
      def foreach(f: A => Unit): Unit = as.foreach(f)
      def iterator: Iterator[A] = as.iterator
    }

  def values[A](as: A*): Lazy[A] =
    Lazy(as)

  def option[A](o: Option[A]): Lazy[A] =
    o match {
      case Some(a) => Lazy.some(a)
      case None => Lazy.none
    }

  def none[A]: Lazy[A] =
    new Lazy[A] {
      def foreach(f: A => Unit): Unit = ()
      def iterator: Iterator[A] = Iterator.empty
    }

  def some[A](a: A): Lazy[A] =
    new Lazy[A] {
      def foreach(f: A => Unit): Unit = f(a)
      def iterator: Iterator[A] = Iterator(a)
    }

  object compat {
    implicit class LazyIterable[A](lz: Lazy[A]) extends Iterable[A] {
      override def foreach[U](f: A => U): Unit = lz.foreach(f)
      def iterator: Iterator[A] = lz.iterator
    }
  }
}

trait Lazy[A] { self =>
  def foreach(f: A => Unit): Unit

  def iterator: Iterator[A]

  def toList: List[A] = {
    val buf = new mutable.ListBuffer[A]
    foreach(buf += _)
    buf.toList
  }

  def toVector: Vector[A] = {
    val builder = new immutable.VectorBuilder[A]
    foreach(builder += _)
    builder.result
  }

  def toSet: Set[A] = {
    val builder = new mutable.SetBuilder[A, Set[A]](Set.empty)
    foreach(builder += _)
    builder.result
  }

  def toMap[B, C](implicit ev: <:<[A, (B, C)]): Map[B, C] = {
    val builder = new mutable.MapBuilder[B, C, Map[B, C]](Map.empty)
    foreach(t => builder += t.asInstanceOf[(B, C)])
    builder.result
  }

  def map[B](f: A => B): Lazy[B] =
    new Lazy[B] {
      def foreach(g: B => Unit): Unit = self.foreach(f andThen g)
      def iterator: Iterator[B] = self.iterator.map(f)
      override def map[C](g: B => C): Lazy[C] = self.map(f andThen g)
      override def flatMap[C](g: B => Lazy[C]): Lazy[C] = self.flatMap(f andThen g)
      override def filter(g: B => Boolean): Lazy[B] = self.filter(f andThen g).map(f)
    }

  def flatMap[B](f: A => Lazy[B]): Lazy[B] =
    new Lazy[B] {
      def foreach(g: B => Unit): Unit = self.foreach(f(_).foreach(g))
      def iterator: Iterator[B] = self.iterator.flatMap(f(_).iterator)
      override def map[C](g: B => C): Lazy[C] = self.flatMap(f(_).map(g))
      override def flatMap[C](g: B => Lazy[C]): Lazy[C] = self.flatMap(f(_).flatMap(g))
      override def filter(g: B => Boolean): Lazy[B] = self.flatMap(f(_).filter(g))
    }

  def filter(f: A => Boolean): Lazy[A] =
    new Lazy[A] {
      def foreach(g: A => Unit): Unit = self.foreach(a => if (f(a)) g(a) else ())
      def iterator: Iterator[A] = self.iterator.filter(f)
      override def filter(g: A => Boolean): Lazy[A] = self.filter(a => f(a) && g(a))
    }

  def take(n: Int): Lazy[A] =
    new Lazy[A] {
      def foreach(f: A => Unit): Unit =
        self.iterator.take(n).foreach(f)
      def iterator: Iterator[A] =
        self.iterator.take(n)
    }

  def drop(n: Int): Lazy[A] =
    new Lazy[A] {
      def foreach(f: A => Unit): Unit =
        self.iterator.drop(n).foreach(f)
      def iterator: Iterator[A] =
        self.iterator.drop(n)
    }

  def ++(that: Lazy[A]): Lazy[A] =
    new Lazy[A] {
      def foreach(f: A => Unit): Unit = {
        self.foreach(f)
        that.foreach(f)
      }
      def iterator: Iterator[A] =
        self.iterator ++ that.iterator
      override def map[B](f: A => B): Lazy[B] =
        self.map(f) ++ that.map(f)
      override def flatMap[B](f: A => Lazy[B]): Lazy[B] =
        self.flatMap(f) ++ that.flatMap(f)
      override def filter(f: A => Boolean): Lazy[A] =
        self.filter(f) ++ that.filter(f)
    }

  def :+(rhs: A): Lazy[A] =
    this ++ Lazy.some(rhs)

  def +:(lhs: A): Lazy[A] =
    Lazy.some(lhs) ++ this

  def exists(p: A => Boolean): Boolean =
    iterator.exists(p)

  def forall(p: A => Boolean): Boolean =
    iterator.forall(p)

  def find(p: A => Boolean): Option[A] =
    iterator.find(p)

  def foldLeft[B](init: B)(f: (B, A) => B): B = {
    var b = init
    foreach { a => b = f(b, a) }
    b
  }

  def partition(f: A => Boolean): (Lazy[A], Lazy[A]) =
    (self.filter(f), self.filter(a => !f(a)))

  def zip[B](that: Lazy[B]): Lazy[(A, B)] =
    new Lazy[(A, B)] {
      def foreach(f: ((A, B)) => Unit): Unit =
        iterator.foreach(f)
      def iterator: Iterator[(A, B)] =
        self.iterator zip that.iterator
    }
}
