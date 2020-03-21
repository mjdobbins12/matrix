package matrcomp

object VecMeth {
    implicit class VectorMethods(x: Vector[Int]) {
        def +(y: Vector[Int]) = {
            (x, y).zipped.map(_ + _)
        }

        def dotProduct(y: Vector[Int]): Int = {
            val elements = x zip y
            elements.map(el => el._1 * el._2).fold(0)(_ + _)
        }

        def saxpy(scalar: Int, y: Vector[Int]): Vector[Int] = {
            ???
        }
    }
}

object Main extends App {
    import VecMeth._
    val m = Vector(1, 2, 3)
    val n = Vector(4, 5, 6)
    val res = m + n

    println(res)
}