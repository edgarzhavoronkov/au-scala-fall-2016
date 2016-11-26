// Peano numbers
// data Nat = Zero | Succ Nat
sealed trait Nat {
  def Suc[N](n : N) : Succ[N] = Succ(n)
}

object Nat {
  type Suc[+N <: Nat] = Succ[N]
  type Zero = Zero.type
}

case object Zero extends Nat

case class Succ[+N <: Nat](n: N) extends Nat {

}

// HList = HNil | Cons(T, HList)
sealed trait HList {
  def ::[H](h: H): HCons[H, this.type] = HCons(h, this)
}

object HList {
  type ::[+H, +T <: HList] = HCons[H, T]
  type HNil = HNil.type

  import Nat._

  trait Appendable[L <: HList, R <: HList, Res <: HList] {
    def apply(l: L, r: R): Res
  }

  trait Splittable[N <: Nat, L <: HList, Before <: HList, After <: HList] {
    def apply(n : N, l: L): (Before, After)
  }

  object Appendable {
    //H :: Res == (H :: L) append R
    //       < == >
    //Res == L append R
    //База: R == HNil append R
    implicit def base[R <: HList]: Appendable[HNil, R, R] = new Appendable[HNil, R, R] {
      override def apply(l: HNil, r: R): R = r
    }

    implicit def step[H, L <: HList, R <: HList, Res <: HList](implicit appendable: Appendable[L, R, Res]): Appendable[H :: L, R, H :: Res] = {
      new Appendable[H :: L, R, H :: Res] {
        override def apply(l: H :: L, r: R): H :: Res = {
          l.head :: appendable.apply(l.tail, r)
        }
      }
    }
  }

  object Splittable {
    // splitAt Succ[N] H :: L == (H :: (splitAt N L)._1 , (splitAt N L)._2)
    implicit def base[L <: HList]: Splittable[Zero, L, HNil, L] = new Splittable[Zero, L, HNil, L] {
      override def apply(n: Zero, l: L): (HNil, L) = (HNil, l)
    }

    implicit def step[N <: Nat, H, L <: HList, Before <: HList, After <: HList]
    (implicit splittable: Splittable[N, L, Before, After]) = new Splittable[Suc[N], H :: L, H :: Before, After] {
      override def apply(n: Suc[N], l: H :: L): (H :: Before, After) = splittable(n.n, l.tail) match {
        case (before, after) => (HCons(l.head, before), after)
      }
    }
  }

  def append[L <: HList, R <: HList, Res <: HList](l: L, r: R)
                                                  (implicit appendable: Appendable[L, R, Res]): Res = {
    appendable(l, r)
  }

  def splitAt[N <: Nat, L <: HList, Before <: HList, After <: HList](idx: N, l : L)
                                                                    (implicit splittable: Splittable[N, L, Before, After]) : (Before, After) = {
    splittable(idx, l)

  }
}

case object HNil extends HList

case class HCons[+H, +T <: HList](head: H, tail: T) extends HList {

}

import HList._
import Nat._

val t: String :: Int :: HNil = "text" :: 1 :: HNil
val r: String :: Boolean :: HNil = "text2" :: false :: HNil

append(t, r).tail.head

//splitAt(Succ[Zero], t)

val x = Succ(Zero)

