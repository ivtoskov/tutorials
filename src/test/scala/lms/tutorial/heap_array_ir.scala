package lms.tutorial

import scala.lms.common._
import scala.reflect.SourceContext

trait HeapArrayOps extends Variables {
  implicit def repHeapArrayToHeapArrayOps[T:Typ](a: Rep[Array[T]]) : HeapArrayOpsCls[T] = new HeapArrayOpsCls(a)

  class HeapArrayOpsCls[T:Typ](a: Rep[Array[T]]) {
    def apply(n: Rep[Int])(implicit pos: SourceContext) = heapArray_apply(a, n)
    def update(n: Rep[Int], y: Rep[T])(implicit pos: SourceContext) = heapArray_update(a, n, y)
  }

  object NewHeapArray {
    def apply[T:Typ](size: Rep[Int]) = heapArray_new[T](size)
  }

  def heapArray_new[T:Typ](size: Rep[Int]): Rep[Array[T]]
  def heapArray_apply[T:Typ](x: Rep[Array[T]], n: Rep[Int])(implicit pos: SourceContext): Rep[T]
  def heapArray_update[T:Typ](x: Rep[Array[T]], n: Rep[Int], y: Rep[T])(implicit pos: SourceContext): Rep[Unit]
}

trait HeapArrayOpsExp extends HeapArrayOps with EffectExp with VariablesExp {
  case class HeapArrayNew[T:Typ](size: Exp[Int]) extends Def[Array[T]] {
    val m = manifest[T]
  }
  case class HeapArrayApply[T:Typ](arr: Exp[Array[T]], index: Exp[Int]) extends Def[T]
  case class HeapArrayUpdate[T:Typ](arr: Exp[Array[T]], index: Exp[Int], elem: Exp[T]) extends Def[Unit]

  implicit def heapArrayTyp[T:Typ]: Typ[Array[T]] = {
    implicit val ManifestTyp(m) = typ[T]

    manifestTyp
  }

  def heapArray_new[T:Typ](size: Exp[Int]) = reflectMutable(HeapArrayNew[T](size))
  def heapArray_apply[T:Typ](arr: Exp[Array[T]], index: Exp[Int])(implicit pos: SourceContext): Exp[T] = HeapArrayApply(arr, index)
  def heapArray_update[T:Typ](arr: Exp[Array[T]], index: Exp[Int], elem: Exp[T])(implicit pos: SourceContext) = reflectWrite(arr)(HeapArrayUpdate(arr,index,elem))
}

trait Fibonacci extends HeapArrayOpsExp with PrimitiveOpsExp with LiftNumeric with IfThenElseExp with OrderingOpsExp with BooleanOpsExp with RangeOpsExp {
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
}

