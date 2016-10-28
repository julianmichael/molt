package molt.util

// trait extended by all (mutable and immutable) union-find data structures
trait UnionFind[A] {
  // returns the data structure with the new element, even if it's modified in
  // place anyway
  def add(a: A): UnionFind[A]
  // returns None if "a" is not in the data structure at all
  def find(a: A): Option[A]
  // returns None if either "a" or "b" is not in the data structure at all
  def union(a: A, b: A): Option[UnionFind[A]]
}

import scala.collection.immutable.SortedSet
// basic, inefficient immutable union-find based on sets
class SetUnionFind[A] private (
  val set: Set[SortedSet[A]])
  (implicit ord: Ordering[A])
  extends UnionFind[A] {

  override def add(a: A): SetUnionFind[A] =
    if(set.forall(!_.contains(a))) new SetUnionFind(set + SortedSet[A](a))
    else this

  override def find(a: A) = for {
    subset <- set.find(_.contains(a))
  } yield subset.head

  override def union(a: A, b: A): Option[SetUnionFind[A]] = for {
    aSet <- set.find(_.contains(a))
    bSet <- set.find(_.contains(b))
    } yield if(aSet != bSet) {
      new SetUnionFind(set - aSet - bSet + (aSet ++ bSet))
    }
    else this

  def representatives: Set[A] = set.map(_.head)
}
object SetUnionFind {
  def empty[A](implicit ord: Ordering[A]): SetUnionFind[A] =
    new SetUnionFind(Set[SortedSet[A]]())
}