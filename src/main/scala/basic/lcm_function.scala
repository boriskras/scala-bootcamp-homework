package basic

import scala.collection.mutable.ListBuffer

object lcm_function {

  def main(args: Array[String]): Unit = {
    def lcm (a: Int, b: Int): Int = {
      var a1 = a
      var b1 = b
      //
      var natVal = (Val : Int) => (2 to Val).collect { case i if Val % i == 0 => i }.reduceLeft(_ min _)
      var A = List[Int] ()
      var B = List[Int] ()
      //
      do {
        A = natVal(a1) :: A
        a1 = a1 / natVal(a1)
      }while(a1>1)

      do {
        B = natVal(b1) :: B
        b1 = b1 / natVal(b1)
      }while(b1>1)

      val A1 = ListBuffer.empty ++= A
      val B1 = ListBuffer.empty ++= B

      var OutPut = List[Int] ()
      for ( i <- A1 ; if B1.contains(i)){
        B1 -= i
        OutPut = i :: OutPut
      }
      var pr = 1
      for (x <- 0 until OutPut.length) {
        pr *= OutPut(x)
      }
      return pr
    }
    println(lcm(64,28))
  }

}
