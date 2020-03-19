package matrcomp

object VecMeth {
    implicit class VectorMethods(x: Vector[Int]) {
        def dotProduct(y: Vector[Int]): Int = {
            val elements = x zip y
            elements.map(el => el._1 * el._2).fold(0)(_ + _)
        }
    }
}

object Main extends App {
    import VecMeth._
    val m = Vector(1, 2, 3)
    val n = Vector(4, 5, 6)
    val res = m dotProduct n

    println(res)
}