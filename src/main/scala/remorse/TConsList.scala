package remorse

sealed abstract class TConsList[C[_, _], X, Y]

object TConsList extends TConsListInstances {

  final case class CNil[C[_, _], X]() extends TConsList[C, X, X]
  abstract case class Cons[C[_, _], X, Y, Z]() extends TConsList[C, X, Z] {
    type Y
    def l: C[X, Y]
    def r: TConsList[C, Y, Z]
  }

  def cons[C0[_, _], X0, Y0, Z0](l0: C0[X0, Y0], r0: TConsList[C0, Y0, Z0]): TConsList[C0, X0, Z0] =
    new Cons[C0, X0, Y0, Z0] {
      type Y = Y0
      def l = l0
      def r = r0
    }
}

sealed abstract class TConsListInstances {
  import TConsList._

  val tsequence: TSequence[TConsList] =
    new TSequence[TConsList] {
      override def empty[C[_, _], X]: TConsList[C, X, X] = CNil()
      override def singleton[C[_, _], X, Y](c: C[X, Y]): TConsList[C, X, Y] = cons(c, CNil())
      override def addL[C[_, _], X, Y, Z](l: C[X, Y], r: TConsList[C, Y, Z]): TConsList[C, X, Z] = cons(l, r)
      override def viewL[C[_, _], X, Y](l: TConsList[C, X, Y]) =
        l match {
          case n @ CNil() => TViewL.tEmptyL[TConsList, C, X].cast2(???) // TODO
          case c @ Cons() => TViewL.tOnL(c.l, c.r)
        }
    }
}
