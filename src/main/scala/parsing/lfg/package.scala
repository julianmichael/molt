package parsing

import scalaz._
import Scalaz._

package object lfg {
  type Feature = String
  type Value = String
  type Specification = Set[Equation[RelativeIdentifier]]
  type FDescription = Set[Equation[AbsoluteIdentifier]]
  type LexicalEntry = (String, Specification)

  // MonadPlus typeclass to give us guards on SolutionState in Solution! Huzzah!
  private trait StateTMonadPlus[F[+_], S]
    extends MonadPlus[({type λ[α] = StateT[F, S, α]})#λ] {
    implicit def F: MonadPlus[F]

    private type StateTForThisState[G[+_], A] = StateT[G, S, A]
    def empty[A]: StateT[F, S, A] = (F.empty[A]).liftM[StateTForThisState]
    def plus[A](
        a: StateT[F, S, A],
        b: => StateT[F, S, A]): StateT[F, S, A] =
      StateT(s => F.plus(a.run(s), b.run(s)))

    def bind[A, B](
        fa: StateT[F, S, A])
        (f: A => StateT[F, S, B]): StateT[F, S, B] =
      fa.flatMap(f)

    def point[A](a: => A): StateT[F, S, A] = {
      lazy val aa = a
      StateT(s => F.point(s, aa))
    }
  }
  implicit def stateTMonadPlus[F[+_], S](
      implicit F0: MonadPlus[F]): MonadPlus[({type λ[α] = StateT[F, S, α]})#λ] =
    new StateTMonadPlus[F, S] {
      implicit def F: MonadPlus[F] = F0
  }
}

