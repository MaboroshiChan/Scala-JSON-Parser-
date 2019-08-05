package Demo


import netscape.javascript.JSObject

object main {
  def Seg(): Unit ={
    var arr:Array[Double] = Array(1.98,2.2,3.1,5.6,7,9,4,11.667)
    var Tre:SegTree[Double] = new SegTree(arr, Math.max)
    Tre.make_tree()
    Tre.update_tree(1, 4)
    Tre.print_tree()
    Tre.print_arr()
  }
  def main(args:Array[String]):Unit={
    Seg()
  }
}
