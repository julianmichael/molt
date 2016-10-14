package molt
package util

class DependentMap[F[_], G[_]] private (map: Map[F[_], G[_]]) {
  def get[A](key: F[A]): Option[G[A]] = map.get(key).asInstanceOf[Option[G[A]]]
  def put[A](key: F[A], value: G[A]): DependentMap[F, G] = new DependentMap[F, G](map + (key -> value).asInstanceOf[(F[_], G[_])])
  def keys: Iterable[F[_]] = map.keys
  override def toString = map.toString
}
object DependentMap {
  def empty[F[_], G[_]] = new DependentMap[F, G](Map.empty[F[_], G[_]])
}
