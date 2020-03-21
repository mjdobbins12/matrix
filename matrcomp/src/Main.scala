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

        def vectorMultiply(m: Vector[Vector[Int]]): Vector[Int] = {
            m.map(row => row dotProduct x)
        }

        // def gaxpy(m: Vector[Vector[Int]], y: Vector[Int]): Vector[Int] = {
        //     y * m + x
        // }
    }
}

object Main extends App {
    import VectorMethods._
    val m = Vector(Vector(1, 2), Vector(3, 4), Vector(5, 6))
    val n = Vector(7, 8)
    val res = n vectorMultiply m

    println(res)
}