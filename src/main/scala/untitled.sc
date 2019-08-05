import scala.annotation.tailrec



def revere[A](as:List[A]): List[A] = as match{
  case Nil=>Nil
  case x::Nil=>List(x)
  case x::xs=>revere(xs) ::: List(x)
}

revere(List(1,2,3,4,5))



def map[A, B](as:List[A])(f:A=>B):List[B]= as match {
  case Nil=> Nil
  case x::xs=> f(x)::map(xs)(f)
}

map(List(1,2,3,4,5))(_.toString)

def flatmap[A, B](as:List[A])(f:A=>List[B]):List[B] = as match {
  case Nil => Nil
  case x::xs=>f(x) ++ flatmap(xs)(f)
}

flatmap(List(1,2,3))(i=>List(i, i))


sealed trait Tree[+A]
case class Leaf[A](value:A) extends Tree[A]
case class Branch[A](left:Tree[A], right:Tree[A]) extends Tree[A]

val T = Branch(Branch(Leaf(12), Leaf(23)), Branch(Leaf(24), Leaf(1)))

def maxOf(x:Int, y:Int):Int = {
  if(x > y)x else y
}

def maxTreeValue(T:Tree[Int]):Int = T match{
  case Leaf(x)=> x
  case Branch(left, right)=> math.max(maxTreeValue(left), maxTreeValue(right))
}


List(1,2,3,4,5).map(_%2 match{
  case 0 => 0
  case 1 => 1
})



maxTreeValue(T)


def compose[A, B, C](f:A=> B, g:B=>C):A=>C = {
  x=>g(f(x))
}

val f = (x:Int)=>x+0.1
val g = (x:Double)=>x.toString

def check(s:String):Option[String]=s match {
  case "" => None
  case _ => Some(s)
}
@tailrec
def foldLeft[A, B](as:List[A], z:B)(f:(A, B)=>B):B = as match {
  case Nil=>z
  case x::xs => foldLeft(xs, f(x, z))(f)
}

def sum(as:List[Int]):Int = {
  foldLeft(as, 0)(_+_)
}

def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
  foldLeft(l, (b:B) => b)((a,g) => x => g(f(a,x)))(z)




def reverse[A](l:List[A]):List[A] = {
  foldLeft(l, List[A]())((x,y)=>List(x):::y)
}

def concat(strings:List[String]):String = {
  foldLeft(reverse(strings),"")(_+_)
}


concat(List("H","e","l","l","o"))


val fib = (f:Int=>Int)=>(x:Int)=> x match {
  case 1=> 1
  case 2=> 1
  case _=>f(x-1) + f(x-2)
}



def Y[A, B](func: (A => B) => A => B): A => B = x=>func(Y(func))(x)


Y(fib)(6)


val fac =  (f:Int=>Int)=>(x:Int)=> if(x == 0) 1 else x * f(x-1)

Y(fac)(5)



val mf = Math.max(_:Int,_:Double)

val map = Map[Int,Int](1->2,2->3)

val map2 = Map[Int,Int](3->4)

trait States
case object S1 extends States
case object S2 extends States
case object S3 extends States

def prog(s:States, data:Int):String = {
  var dat = data
  var st = s
  while(true){
    (st, dat) match {
      case (_:S1.type, 1)=>{
        st = S2
        dat = 2
      }
      case (_:S2.type, 2)=>{
        st = S3
        dat = 3
      }
      case (_:S3.type, 3)=>{
        st = S1
        dat = 1
        return dat.toString
      }
      case (_, _)=>{
        st = S1
        dat = 100
        return dat.toString
      }
    }
  }
  ""
}

prog(S1, 4)

val str = "Tachibana Kanade"

val arr = Array("a", "b")

val brr = arr.map(_.toUpperCase)


val s = " Hello world "

s.reverse.split(" ").filter(!_.equals("")).map(_.reverse).mkString(" ")
