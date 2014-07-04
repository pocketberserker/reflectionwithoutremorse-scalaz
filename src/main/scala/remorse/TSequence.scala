package remorse

import scalaz._

object TSequence extends TSequenceInstances {
}

trait TSequence[S[_[_, _], _, _]] {

  def empty[C[_, _], X]: S[C, X, X]

  def singleton[C[_, _], X, Y](a: C[X, Y]): S[C, X, Y]

  def compose[C[_, _], X, Y, Z](l: S[C, X, Y], r: S[C, Y, Z]): S[C, X, Z] = viewL(l) match {
    case TEmptyL() => r
    case TOnL(h, t) => addL(h, compose(t, r))
  }

  def addL[C[_, _], X, Y, Z](l: C[X, Y], r: S[C, Y, Z]): S[C, X, Z] = compose(singleton(l), r)

  def addR[C[_, _], X, Y, Z](l: S[C, X, Y], r: C[Y, Z]): S[C, X, Z] = compose(l, singleton(r))

  def viewL[C[_, _], X, Y](q: S[C, X, Y]): TViewL[S, C, X, Y] = viewR(q) match {
    case TEmptyR() => TEmptyL()
    case TOnR(p, l) => viewL(p) match {
      case TEmptyL() => TOnL(l, empty)
      case TOnL(h, t) => TOnL(h, addR(t, l))
    }
  }

  def viewR[C[_, _], X, Y](q: S[C, X, Y]): TViewR[S, C, X, Y] = viewL(q) match {
    case TEmptyL() => TEmptyR()
    case TOnL(h, t) => viewR(t) match {
      case TEmptyR() => TOnR(empty, h)
      case TOnR(p, l) => TOnR(addL(h, p), l)
    }
  }
}

sealed trait TViewL[S[_[_, _], _, _], C[_, _], X, Y]
case class TEmptyL[S[_[_, _], _, _], C[_, _], X]() extends TViewL[S, C, X, X]
case class TOnL[S[_[_, _], _, _], C[_, _], X, Y, Z](h: C[X, Y], t: S[C, Y, Z]) extends TViewL[S, C, X, Z]

sealed trait TViewR[S[_[_, _], _, _], C[_, _], X, Y]
case class TEmptyR[S[_[_, _], _, _], C[_, _], X]() extends TViewR[S, C, X, X]
case class TOnR[S[_[_, _], _, _], C[_, _], X, Y, Z](p: S[C, X, Y], l: C[Y, Z]) extends TViewR[S, C, X, Z]

trait TSequenceInstances {

  def category[S[_[_, _], _, _], C[_, _]](implicit S0: TSequence[S]): Category[({ type f[x, y] = S[C, x, y]})#f] =
    new Category[({ type f[x, y] = S[C, x, y]})#f] {
      def id[X]: S[C, X, X] = S0.empty
      def compose[X, Y, Z](r: S[C, Y, Z], l: S[C, X, Y]): S[C, X, Z] = S0.compose(l, r)
    }
}

