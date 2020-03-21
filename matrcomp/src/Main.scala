package matrcomp

object VectorMethods {
    implicit class VecMeth(x: Vector[Int]) {
        
        def +(y: Vector[Int]): Vector[Int] = {
            (x, y).zipped.map(_ + _)
        }

        // multiplication of a vector by a scalar
        def *(a: Int): Vector[Int] = {
            x.map(_ * a)
        }

        def hadamard(y: Vector[Int]): Vector[Int] = {
            (x, y).zipped.map((xi, yi) => xi * yi)
        }

        def dotProduct(y: Vector[Int]): Int = {
            (x hadamard y).fold(0)(_ + _)
        }

        def saxpy(a: Int, y: Vector[Int]): Vector[Int] = {
            y * a + x
        }
    }
}

object MatrixMethods {
    implicit class MatMeth(m: Vector[Vector[Int]]) {
        def gaxpy = ???
    }
}

object Main extends App {
    import VectorMethods._
    val m = Vector(1, 2, 3)
    val n = Vector(4, 5, 6)
    val res = m hadamard n

    println(res)
}