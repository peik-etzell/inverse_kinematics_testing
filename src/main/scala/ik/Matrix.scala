package ik

import scala.math._
import scala.collection.mutable.Buffer
import scala.collection.immutable

class Matrix(val values: Array[Array[Double]]) {
    val n = values.size
    val m = values.head.size

    def apply(i: Int, j: Int): Double = values(i)(j)

    def apply(i: Int): Double = {
        require(isVector)
        values(i)(0)
    }

    def i = {
        require(n <= 3 && isVector)
        apply(0)
    }
    def j = {
        require(n <= 3 && isVector)
        apply(1)
    }
    def k = {
        require(isVector(3))
        apply(2)
    }

    def update(i: Int, j: Int, value: Double): Unit = {
        values(i)(j) = value
    }

    def update(i: Int, value: Double): Unit = {
        require(isVector)
        values(i)(0) = value
    }

    def update(i1: Int, i2: Int, j1: Int, j2: Int, value: Matrix): Unit = {
        var i = i1
        var j = j1
        while (i < i2) {
            while (j < j2) {
                this(i, j) = value(i - i1, j - j1)
                j += 1
            }
            i += 1
            j = j1
        }
    }

    def angle(that: Matrix): Double = {
        
        ???
    }

    def shape = (n, m)

    def length: Double = {
        require(isVector)
        sqrt(this.values.map(arr => arr.head*arr.head).sum)
    }

    def isZero: Boolean = {
        this.length < 1e-7
    }

    def isVector(n: Int): Boolean = {
        this.n == n && this.m == 1
    }

    def isVector: Boolean = {
        this.m == 1
    }

    def subMatrix(i1: Int, i2: Int, j1: Int, j2: Int) = {
        Matrix(values.slice(i1, i2).map(_.slice(j1, j2)))
    }

    def +(that: Matrix) = {
        var i, j = 0
        val v = Array.ofDim[Double](n, m)
        while (i < n) {
            while (j < m) {
                v(i)(j) = this(i, j) + that(i, j)
                j += 1
            }
            i += 1
            j = 0
        }
        Matrix(v)
    }

    def -(that: Matrix): Matrix = {
        this + that*(-1)
    }

    def unary_-() = {
        this*(-1)
    }

    def *(s: Double): Matrix = {
        Matrix(this.values.map(r => r.map(_ * s)))
    }
    
    def *(that: Matrix, inPlace: Boolean = false): Matrix = {
        require(this.m == that.n, "Incompatible matrix multiplication")
        val transpose = that.t
        var i, j, k = 0
        val vals = Array.ofDim[Double](n, that.m)

        while (i < n) {
            while (j < that.m) {
                var v = 0.0
                while (k < m) {                
                    v += this(i, k)*transpose(j, k)
                    k += 1
                }
                vals(i)(j) = v
                j += 1
                k = 0
            }
            i += 1
            j = 0
        }
        if (inPlace) this.setValues(vals)
        Matrix(vals)
    }

    def add(that: Matrix) = {
        // require(this.)
        var i, j = 0
        while (i < n) {
            while (j < m) {
                values(i)(j) += that(i, j)
                j += 1
            }
            i += 1
            j = 0
        }   
    }

    def sub(that: Matrix) = {
        add(that * (-1))
    }

    def mul(s: Double) = {
        for {
            i <- 0 until n
            j <- 0 until m
        } {
            values(i)(j) *= s
        }
    }

    def elemwiseMul(that: Matrix): Matrix = {
        require(this.n == that.n && this.m == that.m)
        var i, j = 0
        val vals = Array.ofDim[Double](n, m)

        while (i < n) {
            while (j < m) {
                vals(i)(j) = this(i, j) * that(i, j)
                j += 1
            }
            i += 1
        }
        Matrix(vals)
    }

    def elemwiseMulInPlace(that: Matrix): Unit = {
        var i, j = 0
        while (i < n) {
            while (j < m) {
                values(i)(j) = this(i, j) * that(i, j)
                j += 1
            }
            i += 1
        }
    }

    def mulRow(i: Int, s: Double) = {
        values(i) = values(i).map(_ * s)
    }

    def addRow(from: Int, to: Int, s: Double) = {
        for (j <- 0 until m) {
            values(to)(j) += values(from)(j) * s 
        }
    }

    def t: Matrix = {
        val v = Array.ofDim[Double](m, n)
        var i, j = 0
        while (i < n) {
            while (j < m) {
                v(j)(i) = this(i, j)
                j += 1
            }
            i += 1
            j = 0
        }
        Matrix(v)
    }

    def setValues(vals: Array[Array[Double]]): Unit = {
        require(vals.length == n && vals.head.length == m)
        vals.copyToArray(values)
    }

    def setValues(mat: Matrix): Unit = {
        setValues(mat.values)
    }

    def row(i: Int) = {
        Vec(values(i)).t
    }

    def column(j: Int) = {
        // Vec(values.map(_.apply(j)))

        Matrix(values.map(arr => Array(arr.apply(j))))
    }

    def |(vec: Matrix): Matrix = {
        require(vec.isVector)
        val v = Array.ofDim[Double](n, m + 1)
        (0 until n).par.foreach(i => {
            values(i).copyToArray(v(i))
            v(i)(m) = vec(i)
        })
        new Matrix(v)
    }

    def setCol(j: Int, vec: Vec): Unit = {
        for (i <- 0 until n) {
            this.values(i)(j) = vec(i)
        }
    }

    def cols(from: Int, until: Int) = {
        new Matrix(values.map(_.slice(from, until)))
    }

    def reduce() = {
        var i, j = 0

        while (i < n && j < m) {
            var k = i + 1
            while (this(i, j) == 0 && k < n) {
                // if pivot == 0, change from below 
                if (this(k, i) != 0) {
                    val tmp = values(i)
                    values(i) = values(k)
                    values(k) = tmp
                }
                k += 1
            }
            if (this(i, j) == 0) j += 1 
            else {                
                val row = this.row(i)
                if (this(i, j) != 0 && !(this(i, i).isInfinity || this(i, i).isNaN)) { 
                    for (w <- 0 until n) {
                        // pivot elem
                        if (w == i) {
                            mulRow(i, 1/this(i, j))
                        } else {
                            addRow(i, w, -this(w, j)/this(i, j))
                        }
                    }
                }
                i += 1
                j += 1
            }
        }
    }

    def solve(aux: Matrix) = {
        require(this.m == aux.n+1)
        this.reduce()
        
        for (i <- 0 until n) {
            val j = this.values(i).indexWhere(_ != 0)
            if (j != -1) {
                aux(j, 0) = this(i, m-1)
                for (k <- j+1 until m-1) {
                    val elem = this(i, k)
                    if (elem.abs > 1e-10) {
                        aux(k, k) = 1
                        aux(i, k) = -elem
                    }
                }
            }
        }

    }


    // Old, less efficient
    def solve(equals: Vec): Array[Vec] = {
        require(this.n == equals.n)
        val concat = this|equals
        concat.reduce()
        val res = Array.fill(m, m)(0.0)
        
        for (i <- 0 until n) {
            val j = concat.values(i).indexWhere(_ != 0)
            if (j != -1) {
                // set constant vector's value
                res(0)(j) = concat(i, m)
                for (k <- j+1 until m) {
                    val elem = concat(i, k)
                    if (elem.abs > 1e-10) {
                        res(k)(k) = 1
                        res(k)(i) = -elem
                    }
                }
            }
        }
        res.filter(_.exists(_.abs > 1e-12)).map(Vec(_))
    }

    def toVec = {
        require(m == 1)
        Vec(values)
    }
    
    def toScalar = {
        require(n == 1 && m == 1) 
        values.head.head
    }

    override def toString(): String = {
        var i, j = 0
        // val max = values.map(_.map(_.toString().length()).max).max
        var s = ""
        while (i < n) {
            while (j < m) {
                s += ("%-10g ").format(this(i, j))
                j += 1
            }
            s += "\n"
            i += 1
            j = 0
        }
        s
    }
}

// Moving away from subclass Vec, using Matrix(n, 1) instead as column vectors
class Vec(vals: Array[Array[Double]]) extends Matrix(vals) {
    def x(that: Vec): Vec = {
        require(this.n == 3 || that.n == 3)
        Vec(
            this.j * that.k - this.k * that.j,
            this.i * that.k - this.k * that.i,
            this.i * that.j - this.j * that.i
        )
    }
    
    def scale(l: Double) = {
        this.mul(l/length)
    }

    def normalize() = {
        this.scale(1.0)
    }
}

object Vec {
    def apply(vals: Double*) = {  
        new Vec(vals.toArray.map(Array(_)))
    }

    def apply(vals: Array[Double]) = {
        new Vec(vals.map(Array(_)))
    }

    def apply(vals: Array[Array[Double]]) = {
        new Vec(vals)
    }

    def zero(n: Int) = {
        new Vec(Array.fill(n, 1)(0.0))
    }

}

object Matrix {
    def apply(values: Array[Array[Double]]) = new Matrix(values)
    def apply(n: Int, m: Int) = new Matrix(Array.fill(n, m)(0.0))
}

// Singleton for creating rotation matrices
object R {
    def apply(theta: Double, axis: Char): Matrix = {
        val s = sin(theta)
        val c = cos(theta)
        
        axis.toLower match {
            case 'z' => {
                Matrix(Array(
                    Array(c, -s, 0),
                    Array(s, c, 0),
                    Array(0, 0, 1)
                ))
            }
            case 'y' => {
                Matrix(Array(
                    Array(c, 0, s),
                    Array(0, 1, 0),
                    Array(-s, 0, c)
                ))
            }
            case 'x' => {
                Matrix(Array(
                    Array(1, 0, 0),
                    Array(0, c, -s),
                    Array(0, s, c)
                ))
            }
        }
    }
    
    def apply(axis: Matrix): Matrix = {
        require(axis.isVector(3))
        R(axis.i, 'x') * R(axis.j, 'y') * R(axis.k, 'z')
    }
}

// Singleton for creating Identity matrices
object I {
    def apply(n: Int, s: Double = 1.0): Matrix = {
        val v = Array.ofDim[Double](n, n)
        var i = 0
        while (i < n) {
            v(i)(i) = s
            i += 1
        }
        Matrix(v)
    }
}
