package lms.tutorial

import scala.lms.common._

class Fibonacci extends HeapArrayOpsExp { q =>
  val codegen = new FibonacciCodeGen {
    val IR: q.type = q
  }

  def fib(n: Rep[Int]) = {
    val arr = NewHeapArray[Int](n)

    if (n > 0)
      arr(0) = 0

    if (n > 1)
      arr(1) = 1

    for (i <- (2 until n): Rep[Range]) {
      arr(i) = arr(i - 1) + arr(i - 2)
    }
    arr
  }

  lazy val code: String = {
    val source = new java.io.StringWriter()
    codegen.emitSource(fib, "fib", new java.io.PrintWriter(source))
    source.toString
  }
}

trait FibonacciCodeGen extends CGenPrimitiveOps with CGenBooleanOps with CGenIfThenElse
    with CGenRangeOps with CGenOrderingOps with CGenVariables {
  val IR: Fibonacci
  import IR._

  def getMemoryAllocString(count: String, memType: String): String = {
    "(" + memType + "*)malloc(" + count + " * sizeof(" + memType + "));"
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@HeapArrayNew(n) =>
      val arrType = remap(a.m)
      stream.println(arrType + "* " + quote(sym) + " = " + getMemoryAllocString(quote(n), arrType))
    case HeapArrayApply(x,n) => emitValDef(sym, quote(x) + "[" + quote(n) + "]")
    case HeapArrayUpdate(x,n,y) => stream.println(quote(x) + "[" + quote(n) + "] = " + quote(y) + ";")
    case _ => super.emitNode(sym,rhs)
  }

  override def remap[A](m: Typ[A]): String = m.toString match {
    case "Array[Int]" => "int32_t*"
    case _ => super.remap(m)
  }

  override def isPrimitiveType(tpe: String) : Boolean = {
    tpe match {
      case "int32_t*" => true
      case _ => super.isPrimitiveType(tpe)
    }
  }

  override def emitSource[A:Typ](args: List[Sym[_]], body: Block[A], functionName: String, out: java.io.PrintWriter) = {
    withStream(out) {
      stream.println("""
      #include <stdio.h>
      #include <stdlib.h>

      #define bool (int)

      void printArray(int32_t * arr, int32_t size) {
        printf("[");
        int i = 0;
        while (i < size) {
          printf("%d", arr[i]);
          i = i + 1;
          if (i < size) {
            printf(", ");
          }
        }

        printf("]\n");
      }

      int32_t * fib(int32_t);

      int main(int argc, char** argv) {
        int size = atoi(argv[1]);
        int32_t * fibArray = fib(size);
        printArray(fibArray, size);
        free(fibArray);
        return 0;
      }

      """)
    }
    super.emitSource[A](args, body, functionName, out)
  }
}
