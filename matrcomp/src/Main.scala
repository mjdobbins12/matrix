package matrcomp

object VectorMethods {
    implicit class VecMeth(x: Vector[Int]) {
        def +(y: Vector[Int]): Vector[Int] = {
            (x, y).zipped.map(_ + _)
        }

        def *(a: Int): Vector[Int] = {
            x.map(_ * a)
        }

        def dotProduct(y: Vector[Int]): Int = {
            val elements = x zip y
            elements.map(el => el._1 * el._2).fold(0)(_ + _)
        }

        def saxpy(a: Int, y: Vector[Int]): Vector[Int] = {
            y * a + x
        }
    }
}

object Main extends App {
    import VectorMethods._
    val m = Vector(1, 2, 3)
    val n = Vector(4, 5, 6)
    val res = m.saxpy(3, n)

    println(res)
}