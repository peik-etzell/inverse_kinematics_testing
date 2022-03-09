package ik

import java.awt.Graphics2D
import scala.collection.mutable.Buffer
import java.awt.Point
import math._
import scala.util.Random

object Arm {
    var debug = ""
    val root = Vec(Canvas.bounds.width / 2, Canvas.bounds.height / 2, 0)

    val n = 2
    val segments = Array.ofDim[Segment](n)
    val angles = Matrix(Array.ofDim(n, 1))
    val lengths = Array(200, 200, 50)

    val jacobian = Matrix(3, n+1)
    val aux = Matrix(n, n)
    val aux2 = Matrix(n, n)
    val zero = Matrix(n, n)
    // val l = Matrix(Array(
    //     Array(lengths(0), 0, 0),
    //     Array(0, lengths(1), 0),
    //     Array(0, 0, lengths(2))
    // ))
    // l.mul(1.0/100)

    var i = n - 1
    while (i >= 0) {
        if (i < n - 1) {
            segments(i) = new Segment(lengths(i), Some(segments(i + 1)))
        } else {
            segments(i) = new Segment(lengths(i))
        }
        // angles(i) == Pi
        i -= 1
    }

    def update() = {
        segments(0).update(root)
    }

    def moveOld(towards: Vec): Unit = {
        update()
        val dx = (towards - segments.head.tip - root).toVec
        dx.mulRow(1, -1)
        dx.scale(5.0 min dx.length)
        debug = dx.toString()
        if (dx.isZero) return 
        // val dx = Vec(1, 0, 0)
        val mat = Matrix(3, n)
        for (j <- 0 until n) {
            mat.setCol(j, segments(j).partialDerivative)
        }   
        // println(mat)

        val sols = mat.solve(dx)

        // ATAx = ATb
        // solve for x: ATA equals ATb
        // A = sols(1:m), b = sols.head
        // val a = Matrix(n, sols.length)
        // for (j <- sols.indices) {
        //     a.setCol(j, sols(j))
        // }
        // val tmp = a.t*l*a
        // val combo = (a.t*a).solve((a.t*sols.head).toVec)
        // val rotation = a*combo.head + sols.head
        // println(tmp)
        for (i <- 0 until n) {
            segments(i).rotate(sols.head(i)/2)
        }
    }

    def f(dx: Matrix): Double = {
        require(dx.isVector(n))
        var i = 0
        var s = 0.0
        while (i < n) {
            s -= dx(i)*segments(i).tip.length
            i += 1
        }
        s
    }

    def move(towards: Vec): Unit = {
        update()
        val dx = (towards - segments.head.tip - root).toVec
        dx.mulRow(1, -1)
        dx.scale(10.0 min dx.length)
        // debug = dx.toString()
        if (dx.isZero) return     

        for (j <- 0 until n) {
            jacobian(0, 3, j, j+1) = segments(j).partialDerivative
        }
        jacobian(0, 3, n, n+1) = dx

        // aux(0, n, 0, n) = zero

        jacobian.solve(aux)
        // val solution, solCopy = aux.column(0)

        // aux(0, 3, 0, 1) = Vec.zero(3)

        // for (i <- 0 until n) {
        //     aux.mulRow(i, segments(i).tip.length)
        //     solCopy.mulRow(i, segments(i).tip.length)
        // }

        // ((aux.t*aux)|(aux.t*solCopy)).solve(aux2)

        // debug = aux.column(0).toString()
        
        // val gamma = 1e-10
        debug = segments(0).tip.toString()


        for (i <- 0 until n) {
            // Dividing by 2, stuff breaks otherwise
            segments(i).rotate(aux(i, 0)/2)
        }
    }


    def draw(g: Graphics2D) = {
        segments.foreach(_.draw(g))
        g.drawString(debug, 10, 15)
    }
    
}

class Segment(length: Double, val next: Option[Segment] = None) {
    val dimensions = 3
    
    val vec = Vec(1, Random.nextDouble(), 0)
    vec.scale(length)
    
    val rot = Vec(0, 0, 1)
    val tip = Vec.zero(3)

    var start = Vec.zero(dimensions)
    var end = Vec.zero(dimensions)

    def draw(g: Graphics2D): Unit = {
        g.drawLine(start.i.toInt, Canvas.height - start.j.toInt, end.i.toInt, Canvas.height - end.j.toInt)
    }

    def update(st: Vec): Unit = {
        start.setValues(st)
        end.setValues(start + vec)
        next.foreach(_.update(end))
        this.tip.setValues({
            next match {
                case Some(seg) => this.vec + seg.tip
                case None =>  this.vec
            }
        })
    }

    def partialDerivative: Vec = {
        (rot x tip).toVec
    }

    def rotate(theta: Double): Unit = {
        val rotationMatrix = R((rot*theta).toVec)
        this.rotate(rotationMatrix)
    }

    def rotate(r: Matrix): Unit = {
        vec.setValues(r*vec)
        rot.setValues(r*rot)
        next.foreach(_.rotate(r))
    }
}