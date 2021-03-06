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
            x.map(i => y * i)

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

        def isSquare: Boolean = m.size == m(0).size

        def isDiagonal: Boolean = {
            var res = true
            for (i <- 0 until m.size; j <- 0 until m.size) {
                if ((m(i)(j) == 0) && i == j) {
                    res = false
                } else if ((m(i)(j) != 0) && i != j) {
                    res = false
                }
            }
            res
        }

        def isSymmetrical: Boolean =
            m == m.transpose
    }
}

object Main extends App {
    import VectorMethods._
    val m = Vector(Vector(1, 2, 3), Vector(2, 4, 5), Vector(3, 5, 6), Vector(3, 5, 6))
    val res = m.isSquare
    
    println(res)
}