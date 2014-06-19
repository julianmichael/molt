import scala.collection.mutable
package object util {
  /*
   * Used to memoize a deterministic function of 1 param.
   * 
   * ASSUMPTION: It's deterministic, side effects will
   * only need to apply upon 1st calculation if at all
   * 
   * Usage:
   * val memoizedFunc = Memoize(func)
   * 
   * For recursive functions, write a generator function
   * that takes the recursive call as a (curried) parameter
   * funcGen: (T => R) => T => R
   * lazy val memoizedFunc = Memoize(funcGen(memoizedFunc))
   */
  class Memoize[-T, +R](f: T => R) extends (T => R) {
    import scala.collection.mutable
    private[this] val vals = mutable.Map.empty[T, R]

    def apply(x: T): R = {
      if (vals.contains(x)) {
        vals(x)
      } else synchronized {
        if (vals.contains(x))
          vals(x)
        else {
          val y = f(x)
          vals += ((x, y))
          y
        }
      }
    }
  }
  object Memoize {
    def apply[T, R](f: T => R) = new Memoize(f)
  }
}
