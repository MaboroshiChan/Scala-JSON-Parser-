def apply[A, B](f:A=>B, x:A):B = f(x)


val A = List("1", "2", "3")


val alist = List("Chu", "Shin", "San")

alist.map(_.toUpperCase)

alist.flatMap(_.toUpperCase)

alist.reduce(_ concat _)

val ma = Map()

ma.updated()