package remorse

import scalaz._

trait Sequence[S[_]] {

  def empty[A]: S[A]

  def singleton[A](a: A): S[A]

  def compose[A](l: S[A], r: S[A]): S[A] = viewL(l) match {
    case EmptyL() => r
    case OnL(h, t) => addL(h, compose(t, r))
  }

  def addL[A](l: A, r: S[A]): S[A] = compose(singleton(l), r)

  def addR[A](l: S[A], r: A): S[A] = compose(l, singleton(r))

  def viewL[A](q: S[A]): ViewL[S, A] = viewR(q) match {
    case EmptyR() => EmptyL()
    case OnR(p, l) => viewL(p) match {
      case EmptyL() => OnL(l, empty)
      case OnL(h, t) => OnL(h, addR(t, l))
    }
  }

  def viewR[A](q: S[A]): ViewR[S, A] = viewL(q) match {
    case EmptyL() => EmptyR()
    case OnL(h, t) => viewR(t) match {
      case EmptyR() => OnR(empty, h)
      case OnR(p, l) => OnR(addL(h, p), l)
    }
  }
}

sealed trait ViewL[S[_], A]
case class EmptyL[S[_], A]() extends ViewL[S, A]
case class OnL[S[_], A](h: A, t: S[A]) extends ViewL[S, A]

sealed trait ViewR[S[_], A]
case class EmptyR[S[_], A]() extends ViewR[S, A]
case class OnR[S[_], A](p: S[A], l: A) extends ViewR[S, A]

