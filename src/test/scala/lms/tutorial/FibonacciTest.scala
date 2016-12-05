package scala.lms.tutorial

import lms.tutorial.Fibonacci

/**
  * A simple test class to explore the different ways
  * to generate C code for allocating a heap array and
  * filling it with the Fibonacci sequence.
  *
  * @author Ivaylo Toskov (itoskov@student.ethz.ch)
  */
class FibonacciTest extends TutorialFunSuite {
  val under = "fibonacci"

  test("dslapi-version") {
    val snippet = new DslDriverC[Int, Array[Int]] {
      def snippet(x: Rep[Int]) = {
        val arr = NewArray[Int](x)

        if (x > 0)
          arr(0) = 0

        if (x > 1)
          arr(1) = 1

        for (i <- (2 until x): Rep[Range]) {
          arr(i) = arr(i - 1) + arr(i - 2)
        }
        arr
      }
    }

    check("dslapi-version", snippet.code, "c")
  }

  test("separate-version") {
    val fibFunc = new Fibonacci().code
    println(indent(fibFunc))
    println("weird test done")
  }
}
