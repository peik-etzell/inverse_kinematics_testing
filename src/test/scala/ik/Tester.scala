package ik

import scala.util.Random
import scala.math._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Tester extends AnyFlatSpec with Matchers {
    val rand = new Random(42)

    val M = Matrix(
        Array(
            Array(1, 2, 3), 
            Array(4, 5, 6),
            Array(7, 8, 9)
        )
    )

    val V = Vec(1, 2, 3)
    val U = Vec(1, 2, 3, 4)

    val e1 = Vec(1, 0, 0)
    val e2 = Vec(0, 1, 0)
    val e3 = Vec(0, 0, 1)

    val T = Matrix(Array.fill(3, 5)(rand.nextDouble())) * 10

    assert{M.shape == (3, 3)}
    assert{V.shape == (3, 1)}
    assert{(M*V).shape == (3, 1)}
    assert{(e1 x e2).toVec.length == 1}
    assert((R(Pi/2, 'z')*e1 - e2).toVec.length < 0.1)
    assert((I(3) * V - V).toVec.length < 0.1)
    
    // println(U.length)

    "Vector operations" should "work as expected" in {
        V.scale(1)
        V.length should be (1.0)
    }
    // M.multInPlace(10)
    // M.subInPlace(V*V.t)
    val exp = M|V

    println("T\n" + T)
    println("V\n" + V)
    println("T|V\n" + exp)
    exp.reduce()
    println("Reduced:\n" + exp)

    val sol = T.solve(V)
    println("Solutions:")
    sol.foreach(println)

    val testJacobian = Matrix(Array(
        Array(-18.6801, -12.3820, -11.8926, -2.34521),
        Array(-750.000, -450.000, -250.000, -100.000),
        Array(0.00000, 0.00000, 0.00000, 0.00000))
    )

    val testJacobian2 = Matrix(Array(
        Array(-18.6801, -12.3820, -11.8926, -2.34521),
        Array(-750.000, -450.000, -250.000, -100.000))
    )

    val testDx = Vec(9.98096, 0.616794, 0.00000)
    val testDx2 = Vec(9.98096, 0.616794)

    val sol2 = testJacobian.solve(testDx)
    sol2.foreach(println)

    testJacobian.reduce()
    println(testJacobian)

    val sol3 = testJacobian2.solve(testDx2)
    sol3.foreach(println)

}

class Solver extends AnyFlatSpec with Matchers {
    val rand = new Random(42)
    
    "A constrained solve()" should "work correctly" in {
        val n, m = 3
        val M = Matrix(Array.fill(n, m)(rand.nextDouble()))
        val V = Vec(Array.fill(n)(rand.nextDouble()))
        val sols = M.solve(V)
        sols.foreach(println)

        sols.length should be (1)
        ((M*sols.head - V).toVec.length < 1e-14) should be (true)

    }

    "An underconstrained solve()" should "work correctly" in {
        val n = 3
        val m = 5
        val M = Matrix(Array.fill(n, m)(rand.nextDouble()))
        val V = Vec(Array.fill(n)(rand.nextDouble()))
        val sols = M.solve(V)
        sols.length should be (3)
        ((M*sols.head - V).toVec.length < 1e-14) should be (true)
    }
    
    "solve()" should "handle problematic cases" in {
        val problematicMatrices = Array(
            Matrix(Array(
                Array(1, 2, 1, 0),
                Array(0, 0, 1, 0),
                Array(0, 0, 1, 2)
            )), Matrix(Array(
                Array(1, 2, 1, 0),
                Array(0, 0, 1, 0),
                Array(0, 1, 1, 2)
            ))
        )
        
        val V = Vec(2, 1, 3)
        
        for (matrix <- problematicMatrices) {
            val solutions = matrix.solve(V)
            solutions.foreach(println)
            withClue(f"Matrix:\n" + matrix + "\n" + solutions.mkString("\n")) {
                // matrix.reduce()
                // println(matrix)
                for (i <- solutions.indices) {
                    if (i == 0) {
                        withClue(matrix*solutions(i)) {
                            {(matrix*solutions(i) - V).toVec.length < 1e-14} should be (true)
                        }
                    } else {
                        withClue(matrix*solutions(i)) {
                            {(matrix*solutions(i)).toVec.length < 1e-14} should be (true)
                        }
                    }
                }
            }
        }
    }

    "solve()" should "work as expected using aux" in {
        val problematicMatrices = Array(
            Matrix(Array(
                Array(1, 2, 1, 0),
                Array(0, 0, 1, 0),
                Array(0, 0, 1, 2)
            )), Matrix(Array(
                Array(1, 2, 1, 0),
                Array(0, 0, 1, 0),
                Array(0, 1, 1, 2)
            ))
        )


        
        val V = Vec(2, 1, 3)

        
        val toSolve = problematicMatrices(1)|V
        // val aux = Matrix(4, toSolve.m - toSolve.n)
        val aux = Matrix(4, 4)

        println(toSolve)
        toSolve.solve(aux)
        println(aux)

        true should be (true)
    }

    "Update" should "work as expected" in {
        val M = Matrix(3, 3)
        val V = I(3, 2)
        M(0, 3, 0, 3) = V
        // println(V)
        // println(M)
    }

}