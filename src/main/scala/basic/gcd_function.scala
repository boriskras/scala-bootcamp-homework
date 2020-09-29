package basic

import scala.collection.mutable.ListBuffer

object gcd_function {

  def main(args: Array[String]): Unit = {
    def gcm (a: Int, b: Int): Int = {
      var natVal = (Val : Int) => (2 to Val).collect { case i if Val % i == 0 => i }.reduceLeft(_ min _)

      var a1 = a
      var b1 = b
      var A = List[Int] ()
      var B = List[Int] ()

      if( a < b){
        do {
          A = natVal(a1) :: A
          a1 = a1 / natVal(a1)
        }while(a1>1)
        do {
          B = natVal(b1) :: B
          b1 = b1 / natVal(b1)
        }while(b1>1)

      }else{
        do {
          A = natVal(b1) :: A
          b1 = b1 / natVal(b1)
        }while(b1>1)
        do {
          B = natVal(a1) :: B
          a1 = a1 / natVal(a1)
        }while(a1>1)
      }

      var A1 = ListBuffer.empty ++= A
      var B1 = ListBuffer.empty ++= B

      for (i <- A1; if B1.contains(i)){
        A1 -= i
        B1 -= i
      }

      var B2 = ListBuffer.empty ++= A1
      for (i <- B){
        B2 += i
      }

      var pr = 1
      for (x <- 0 until B2.length) {
        pr *= B2(x)
      }
      return pr
    }

    println(gcm(12,9))
    }

}
