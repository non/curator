package curator

import scala.collection.{immutable, mutable}
import scala.reflect.ClassTag

object Fused {
  def apply[A](as: Iterable[A]): Fused[A] =
    new Fused[A] {
      def foreach(f: A => Unit): Unit = as.foreach(f)
      def iterator: Iterator[A] = as.iterator
    }

  def values[A](as: A*): Fused[A] =
    Fused(as)

  def option[A](option: Option[A]): Fused[A] =
    option match {
      case Some(a) => Fused.some(a)
      case None => Fused.none
    }

  def none[A]: Fused[A] =
    new Fused[A] {
      def foreach(f: A => Unit): Unit = ()
      def iterator: Iterator[A] = Iterator.empty
    }

  def some[A](a: A): Fused[A] =
    new Fused[A] {
      def foreach(f: A => Unit): Unit = f(a)
      def iterator: Iterator[A] = Iterator(a)
    }

  object compat {
    implicit class FusedIterable[A](lz: Fused[A]) extends Iterable[A] {
      override def foreach[U](f: A => U): Unit = lz.foreach(f)
      def iterator: Iterator[A] = lz.iterator
    }
  }
}

trait Fused[A] { self =>
  def foreach(f: A => Unit): Unit

  def iterator: Iterator[A]

  def map[B](f: A => B): Fused[B] =
    new Fused[B] {
      def foreach(g: B => Unit): Unit = self.foreach(f andThen g)
      def iterator: Iterator[B] = self.iterator.map(f)
      override def map[C](g: B => C): Fused[C] = self.map(f andThen g)
      override def flatMap[C](g: B => Fused[C]): Fused[C] = self.flatMap(f andThen g)
      override def filter(g: B => Boolean): Fused[B] = self.filter(f andThen g).map(f)
    }

  def flatMap[B](f: A => Fused[B]): Fused[B] =
    new Fused[B] {
      def foreach(g: B => Unit): Unit = self.foreach(f(_).foreach(g))
      def iterator: Iterator[B] = self.iterator.flatMap(f(_).iterator)
      override def map[C](g: B => C): Fused[C] = self.flatMap(f(_).map(g))
      override def flatMap[C](g: B => Fused[C]): Fused[C] = self.flatMap(f(_).flatMap(g))
      override def filter(g: B => Boolean): Fused[B] = self.flatMap(f(_).filter(g))
    }

  def filter(f: A => Boolean): Fused[A] =
    new Fused[A] {
      def foreach(g: A => Unit): Unit = self.foreach(a => if (f(a)) g(a) else ())
      def iterator: Iterator[A] = self.iterator.filter(f)
      override def filter(g: A => Boolean): Fused[A] = self.filter(a => f(a) && g(a))
    }

  def take(n: Int): Fused[A] =
    new Fused[A] {
      def foreach(f: A => Unit): Unit =
        self.iterator.take(n).foreach(f)
      def iterator: Iterator[A] =
        self.iterator.take(n)
    }

  def drop(n: Int): Fused[A] =
    new Fused[A] {
      def foreach(f: A => Unit): Unit =
        self.iterator.drop(n).foreach(f)
      def iterator: Iterator[A] =
        self.iterator.drop(n)
    }

  def ++(that: Fused[A]): Fused[A] =
    new Fused[A] {
      def foreach(f: A => Unit): Unit = {
        self.foreach(f)
        that.foreach(f)
      }
      def iterator: Iterator[A] =
        self.iterator ++ that.iterator
      override def map[B](f: A => B): Fused[B] =
        self.map(f) ++ that.map(f)
      override def flatMap[B](f: A => Fused[B]): Fused[B] =
        self.flatMap(f) ++ that.flatMap(f)
      override def filter(f: A => Boolean): Fused[A] =
        self.filter(f) ++ that.filter(f)
    }

  def :+(rhs: A): Fused[A] =
    this ++ Fused.some(rhs)

  def +:(lhs: A): Fused[A] =
    Fused.some(lhs) ++ this

  def exists(p: A => Boolean): Boolean =
    iterator.exists(p)

  def forall(p: A => Boolean): Boolean =
    iterator.forall(p)

  def find(p: A => Boolean): Option[A] =
    iterator.find(p)

  def collect[B](pf: PartialFunction[A, B]): Fused[B] = {
    val f = pf.lift
    flatMap(a => Fused.option(f(a)))
  }

  def foldLeft[B](init: B)(f: (B, A) => B): B = {
    var b = init
    foreach { a => b = f(b, a) }
    b
  }

  def groupBy[B](f: A => B): Map[B, List[A]] = {
    val m = mutable.Map.empty[B, mutable.ListBuffer[A]]
    foreach { a =>
      m.getOrElseUpdate(f(a), new mutable.ListBuffer[A]) += a
    }
    val builder = immutable.Map.newBuilder[B, List[A]]
    m.foreach { case (b, as) =>
      builder += ((b, as.toList))
    }
    builder.result
  }

  def reduceLeft(f: (A, A) => A): A =
    iterator.reduceLeft(f)

  def partition(f: A => Boolean): (Fused[A], Fused[A]) =
    (self.filter(f), self.filter(a => !f(a)))

  def zip[B](that: Fused[B]): Fused[(A, B)] =
    new Fused[(A, B)] {
      def foreach(f: ((A, B)) => Unit): Unit =
        iterator.foreach(f)
      def iterator: Iterator[(A, B)] =
        self.iterator zip that.iterator
    }

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

  def toArray(implicit ct: ClassTag[A]): Array[A] = {
    val buf = new mutable.ArrayBuffer[A]
    foreach(buf += _)
    buf.toArray
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

  def toStream: Stream[A] =
    iterator.toStream
}
