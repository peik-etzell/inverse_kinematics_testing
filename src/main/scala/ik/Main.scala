package ik

import scala.swing._
import java.awt.Color
import System.nanoTime

object Main extends SimpleSwingApplication {
    val mainFrame = new MainFrame {
        title = "Inverse kinematics testing"
        // menuBar = new MenuBar {
        //     contents += new Button(Action("Hello") {
        //
        //     })
        // }
        contents = Canvas
        this.centerOnScreen()
    }
    def top: Frame = mainFrame

}