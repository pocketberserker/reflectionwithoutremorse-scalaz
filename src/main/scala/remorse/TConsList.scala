package remorse

object TConsList extends TConsListInstances {
}

sealed trait TConsList[C[_, _], X, Y]
case class CNil[C[_, _], X]() extends TConsList[C, X, X]
case class Cons[C[_, _], X, Y, Z](l: C[X, Y], r: TConsList[C, Y, Z]) extends TConsList[C, X, Z]

trait TConsListInstances {

  def tsequence: TSequence[TConsList] =
    new TSequence[TConsList] {
      def empty[C[_, _], X]: TConsList[C, X, X] = CNil()
      def singleton[C[_, _], X, Y](c: C[X, Y]): TConsList[C, X, Y] = Cons(c, CNil())
      def addL[C[_, _], X, Y, Z](l: C[X, Y], r: TConsList[C, Y, Z]): TConsList[C, X, Z] = Cons(l, r)
      def viewL[C[_, _], X, Y](l: TConsList[C, X, Y]): TViewL[TConsList, C, X, Y] =
        l match {
          case CNil() => TEmptyL()
          case Cons(h, t) => TOnL(h, t)
        }
    }
}
