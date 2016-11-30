package scala.lms.tutorial

import scala.lms.common._

/**
  * A simple test class to explore the different ways
  * to generate C code for allocating a heap array and
  * filling it with the Fibonacci sequence.
  *
  * @author Ivaylo Toskov (itoskov@student.ethz.ch)
  */
class FibonacciTest extends TutorialFunSuite {
  val under = "fibonacci"

  test("dslapi-code") {
    val snippet = new DslDriver[Int, Array[Int]] {
      def snippet(x: Rep[Int]) = {
        NewArray[Int](x)
      }
    }

    println(indent(snippet.code))
  }
}
