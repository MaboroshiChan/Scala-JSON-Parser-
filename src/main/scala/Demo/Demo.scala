package Demo

import scala.annotation.tailrec
import scala.reflect.ClassTag

class SegTree[A:ClassTag:Numeric](private var arr:Array[A], private val ev:(A, A)=>A){

  private val tree = Array.fill[A](2*arr.length + 1)(implicitly[Numeric[A]].zero)

  def make_tree(node:Int = 0,start:Int = 0, end:Int = arr.length - 1):Unit = {
    if(start == end){
      tree(node) = arr(start)
    }else{
      val mid: Int = start + (end - start) / 2
      val left_node = 2 * node + 1
      val right_node = 2 * node + 2
      make_tree(left_node, start, mid)
      make_tree(right_node, mid + 1, end)
      tree(node) = ev(tree(left_node), tree(right_node))
    }
  }
  def update_tree(index:Int, value:A, node:Int = 0,start:Int = 0, end:Int = arr.length - 1): Unit ={
    if(start == end){
      arr(index) = value
      tree(node) = value
    }else {
      val mid: Int = start + (end - start) / 2
      val left_node = 2 * node + 1
      val right_node = 2 * node + 2
      if (start <= index && index <= mid) {
        update_tree(index, value,left_node, start, mid)
      } else {
        update_tree(index, value,right_node, mid + 1, end)
      }
      tree(node) = ev(tree(left_node), tree(right_node))
    }
  }
  def query(node: Int,start:Int = 0, end:Int = arr.length-1, L:Int, R:Int):A = {
    if(R < start || L > end){
      implicitly[Numeric[A]].zero
    }else if (L <= start && end <= R ){
      tree(node)
    }else {
      val mid: Int = start + (end - start) / 2
      val left_node = 2 * node + 1
      val right_node = 2 * node + 2
      ev(query(left_node, start, mid, L, R),
        query(right_node, mid + 1, end, L, R))
    }
  }
  def print_tree():Unit = {
    println(tree.mkString(", "))
  }
  def print_arr():Unit = {
    println(arr.mkString(", "))
  }
}

object Demo {
    @tailrec
    def sum(list:List[Int], va:Int)(f:(Int, Int) => Int): Int = list match {
      case Nil => va
      case x::xs =>
           sum(xs, f(x, va))(f)
    }

    def fib(x:Int = 1, y:Int = 1, n:Int):Int = n match {
      case 0 => x
      case _ => fib(y, x+y, n - 1)
    }
}



