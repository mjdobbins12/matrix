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

        def outerProduct(y: Vector[Int]): Vector[Vector[Int]] = 
            x.map( i => y * i)

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
    val m = Vector(1, 2, 3)
    val n = Vector(4, 5)
    val res = m outerProduct n
    
    println(res)
}