package Misc

class Misc {
  trait Monoid[A]{
    def op(a1:A, a2:A):A
    def zero: A
  }


  val stringMonoid:Monoid[String] = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    val zero = ""
  }

  def listMonoid[A]:Monoid[List[A]] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    val zero = Nil
  }


  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 + a2

    override def zero = 0
  }

  val intMultiplication:Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = a1 * a2

    override def zero = 0
  }

  val booleanOr:Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean) = a1 || a2

    override def zero = false
  }
  def endMonoid[A]:Monoid[A=>A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A=>A) = a1.compose(a2)

    override def zero:A=>A = x=>x
  }


  def concatenate[A](as:List[A], m:Monoid[A]):A = {
    as.foldLeft(m.zero)(m.op)
  }

  def curry[A, B, C](f:(A, B)=>C):A => B => C = {
    x:A=>(y:B)=>f(x, y)
  }

  def uncurry[A,B,C](f:A=>B=>C):(A, B)=>C = {
    (x, y)=>f(x)(y)
  }

}
