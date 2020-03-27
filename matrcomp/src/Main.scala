package matrcomp

object VectorMethods {
    implicit class VecMeth(x: Vector[Int]) {
        def +(y: Vector[Int]): Vector[Int] =
            (x, y).zipped.map(_ + _)

        // multiplication of a vector by a scalar
        def *(a: Int): Vector[Int] =
            x.map(_ * a)

        def hadamard(y: Vector[Int]): Vector[Int] =
            (x, y).zipped.map((xi, yi) => xi * yi)

        def dotProduct(y: Vector[Int]): Int =
            (x hadamard y).fold(0)(_ + _)

        def saxpy(a: Int, y: Vector[Int]): Vector[Int] =
            y * a + x

        def matrixMultiply(m: Vector[Vector[Int]]): Vector[Int] =
            m.map(row => row dotProduct x)

        def gaxpy(m: Vector[Vector[Int]], y: Vector[Int]): Vector[Int] =
            (y matrixMultiply m) + x
    }

    implicit class MatrMeth(m: Vector[Vector[Int]]) {
        def +(n: Vector[Vector[Int]]): Vector[Vector[Int]] =
            (m, n).zipped.map(_ + _)

        def *(n: Vector[Vector[Int]]): Vector[Vector[Int]] =
            m.map(row => n.transpose.map(_ dotProduct row))
    }
}

object Main extends App {
    import VectorMethods._
    val m = Vector(Vector(1, 2), Vector(3, 4))
    val n = Vector(Vector(5, 6), Vector(7, 8))
    val res = m * n
    
    println(res)
}