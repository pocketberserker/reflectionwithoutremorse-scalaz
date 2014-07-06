package remorse

sealed abstract class TSnocList[C[_, _], X, Y]

object TSnocList extends TSnocListInstances {

  final case class SNil[C[_, _], X]() extends TSnocList[C, X, X]
  abstract case class Snoc[C[_, _], X, Y, Z]() extends TSnocList[C, X, Z] {
    type Y
    def l: TSnocList[C, X, Y]
    def r: C[Y, Z]
  }

  def snoc[C0[_, _], X0, Y0, Z0](l0: TSnocList[C0, X0, Y0], r0: C0[Y0, Z0]): TSnocList[C0, X0, Z0] =
    new Snoc[C0, X0, Y0, Z0] {
      type Y = Y0
      def l = l0
      def r = r0
    }
}

sealed abstract class TSnocListInstances {
  import TSnocList._

  val tsequence: TSequence[TSnocList] =
    new TSequence[TSnocList] {
      override def empty[C[_, _], X]: TSnocList[C, X, X] = SNil()
      override def singleton[C[_, _], X, Y](c: C[X, Y]): TSnocList[C, X, Y] = snoc(SNil(), c)
      override def addR[C[_, _], X, Y, Z](l: TSnocList[C, X, Y], r: C[Y, Z]): TSnocList[C, X, Z] = snoc(l, r)
      override def viewR[C[_, _], X, Y](r: TSnocList[C, X, Y]) =
        r match {
          case n @ SNil() => TViewR.tEmptyR[TSnocList, C, X].cast2(???) // TODO
          case c @ Snoc() => TViewR.tOnR(c.l, c.r)
        }
    }
}
