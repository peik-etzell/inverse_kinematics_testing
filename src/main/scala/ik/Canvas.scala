package ik

import scala.swing._
import System.nanoTime
import System.currentTimeMillis
import scala.collection.mutable.Buffer
import scala.swing.event.MouseMoved
import scala.swing.event.MouseDragged
import scala.swing.event.MouseClicked
import scala.swing.event.MousePressed
import scala.swing.event.KeyPressed
import scala.swing.event.Key
import scala.swing.event.MouseReleased

object Canvas extends Panel {
    preferredSize = new Dimension(1280, 800)

    def width = this.bounds.width
    def height = this.bounds.height

    var cur = Vec.zero(3)
    listenTo(mouse.moves, mouse.clicks, keys)
    reactions += {
        case MouseMoved(_, point, _) => {
            cur(0, 0) = point.x
            cur(1, 0) = height-point.y
        }

        case KeyPressed(_, Key.W, _, _) => {
            // cur(1, 0) += 1
        }
        case KeyPressed(_, Key.A, _, _) => {
            // cur(0, 0) -= 1
        }
        case KeyPressed(_, Key.S, _, _) => {
            // cur(1, 0) -= 1
        }
        case KeyPressed(_, Key.D, _, _) => {
            // cur(0, 0) += 1
        }
        
    }
    override def paint(g: Graphics2D): Unit = {
        val fps = 30
        g.clearRect(0, 0, width, height)
        val t0 = System.currentTimeMillis()
        Arm.move(cur)
        Arm.draw(g)
        g.drawOval(cur.i.toInt - 5, height - cur.j.toInt - 5, 10, 10)
        val t1 = System.currentTimeMillis()
        Thread.sleep((1000.0/fps-t1+t0).toInt max 0)
        val t2 = System.currentTimeMillis()
        g.drawString(f"${(t2-t0)}ms", 10, 30)
        g.drawString(f"${(1e3/(t2-t0)).toInt} fps", 10, 45)
        // println((1000.0/fps-(t1-t0)/1000).toInt)
        this.repaint()
    }
}