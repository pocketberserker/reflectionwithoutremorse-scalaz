package remorse

import scalaz._
import TViewR._, TViewL._
import scalaz.Leibniz._

object TSequence extends TSequenceInstances {

  private def sym[A, B](ab: A === B): B === A =
    symm[Nothing, Any, A, B](ab)

}

abstract class TSequence[S[_[_, _], _, _]] {
  import TSequence._

  protected final def cast1[C[_, _], X1, X2, Y](
    x: X1 === X2,
    a: S[C, X1, Y]
  ): S[C, X2, Y] =
    x.subst[({type l[a >: ⊥ <: ⊤] = S[C, X1, Y] === S[C, a, Y]})#l](refl).apply(a)

  protected final def cast2[C[_, _], X, Y1, Y2](
    y: Y1 === Y2,
    a: S[C, X, Y1]
  ): S[C, X, Y2] =
    y.subst[({type l[a >: ⊥ <: ⊤] = S[C, X, Y1] === S[C, X, a]})#l](refl).apply(a)

  def empty[C[_, _], X]: S[C, X, X]

  def singleton[C[_, _], X, Y](a: C[X, Y]): S[C, X, Y]

  def compose[C[_, _], X, Y, Z](l: S[C, X, Y], r: S[C, Y, Z]): S[C, X, Z] =
    viewL(l).cata(
      e => cast1(sym(e), r),
      a => addL(a.h, compose(a.t, r))
    )

  def addL[C[_, _], X, Y, Z](l: C[X, Y], r: S[C, Y, Z]): S[C, X, Z] = compose(singleton(l), r)

  def addR[C[_, _], X, Y, Z](l: S[C, X, Y], r: C[Y, Z]): S[C, X, Z] = compose(l, singleton(r))

  def viewL[C[_, _], X, Y](q: S[C, X, Y]): TViewL[S, C, X, Y] =
    viewR(q).cata(
      tEmptyL[S, C, X].cast2,
      r => viewL(r.p).cata(
        (e: X === r.Y) => ???, //TViewL.tOnL(r.l, cast2(e)(empty[C, X])),
        l => TViewL.tOnL(l.h, addR(l.t, r.l))
      )
    )

  def viewR[C[_, _], X, Y](q: S[C, X, Y]): TViewR[S, C, X, Y] =
    viewL(q).cata(
      tEmptyR[S, C, X].cast2,
      l => viewR(l.t).cata(
        (e: l.Y === Y) => ??? , //TViewR.tOnR(empty[C, X], l.h)
        r => TViewR.tOnR(addL(l.h, r.p), r.l)
      )
    )
}

sealed abstract class TViewL[S[_[_, _], _, _], C[_, _], X, Y] {
  final def cata[Z](empty: (X === Y) => Z, nonEmpty: TOnL[S, C, X, Y] => Z): Z =
    this match {
      case TEmptyL() =>
        empty(Leibniz.force[⊥, ⊤, X, Y]) // TODO
      case a @ TOnL() =>
        nonEmpty(a)
    }

  final def cast1[X2](
    x: X === X2
  ): TViewL[S, C, X2, Y] =
    x.subst[({type l[a >: ⊥ <: ⊤] = TViewL[S, C, X, Y] === TViewL[S, C, a, Y]})#l](refl)(this)


  final def cast2[Y2](
    y: Y === Y2
  ): TViewL[S, C, X, Y2] =
    y.subst[({type l[a >: ⊥ <: ⊤] = TViewL[S, C, X, Y] === TViewL[S, C, X, a]})#l](refl)(this)

}

object TViewL {

  final case class TEmptyL[S[_[_, _], _, _], C[_, _], X]() extends TViewL[S, C, X, X]

  def tEmptyL[S[_[_, _], _, _], C[_, _], X]: TViewL[S, C, X, X] =
    TEmptyL[S, C, X]

  abstract case class TOnL[S[_[_, _], _, _], C[_, _], X, Z]() extends TViewL[S, C, X, Z] {
    type Y
    def h: C[X, Y]
    def t: S[C, Y, Z]
  }

  def tOnL[S0[_[_, _], _, _], C0[_, _], X0, Y0, Z0](h0: C0[X0, Y0], t0: S0[C0, Y0, Z0]): TViewL[S0, C0, X0, Z0] =
    new TOnL[S0, C0, X0, Z0] {
      type Y = Y0
      def h = h0
      def t = t0
    }
}

sealed abstract class TViewR[S[_[_, _], _, _], C[_, _], X, Y] {
  final def cata[Z](empty: (X === Y) => Z, nonEmpty: TOnR[S, C, X, Y] => Z): Z =
    this match {
      case TEmptyR() =>
        empty(Leibniz.force[⊥, ⊤, X, Y]) // TODO
      case a @ TOnR() =>
        nonEmpty(a)
    }

  final def cast1[X2](
    x: X === X2
  ): TViewR[S, C, X2, Y] =
    x.subst[({type l[a >: ⊥ <: ⊤] = TViewR[S, C, X, Y] === TViewR[S, C, a, Y]})#l](refl)(this)


  final def cast2[Y2](
    y: Y === Y2
  ): TViewR[S, C, X, Y2] =
    y.subst[({type l[a >: ⊥ <: ⊤] = TViewR[S, C, X, Y] === TViewR[S, C, X, a]})#l](refl)(this)

}

object TViewR {

  final case class TEmptyR[S[_[_, _], _, _], C[_, _], X]() extends TViewR[S, C, X, X]

  def tEmptyR[S[_[_, _], _, _], C[_, _], X]: TViewR[S, C, X, X] =
    TEmptyR[S, C, X]

  abstract case class TOnR[S[_[_, _], _, _], C[_, _], X, Z]() extends TViewR[S, C, X, Z] {
    type Y
    def p: S[C, X, Y]
    def l: C[Y, Z]
  }

  def tOnR[S0[_[_, _], _, _], C0[_, _], X0, Y0, Z0](p0: S0[C0, X0, Y0], l0: C0[Y0, Z0]): TViewR[S0, C0, X0, Z0] =
    new TOnR[S0, C0, X0, Z0] {
      type Y = Y0
      def p = p0
      def l = l0
    }

}

sealed abstract class TSequenceInstances {

  implicit def category[S[_[_, _], _, _], C[_, _]](implicit S0: TSequence[S]): Category[({ type f[x, y] = S[C, x, y]})#f] =
    new Category[({ type f[x, y] = S[C, x, y]})#f] {
      def id[X]: S[C, X, X] = S0.empty
      def compose[X, Y, Z](r: S[C, Y, Z], l: S[C, X, Y]): S[C, X, Z] = S0.compose(l, r)
    }
}

