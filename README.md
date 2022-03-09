# Testing inverse kinematics

Testing out some algorithms on inverse kinematics. 

https://en.wikipedia.org/wiki/Inverse_kinematics

Solving the equation `Jx = b`, where `J` is the Jacobian of the arm and `b` is the delta you want to move the arm, gives a set of solutions to play around with. 
You get out one vector giving the delta, and `m-n+1` free vectors to choose an optimal solution from.

I'm planning to use gradient descent to find an optimal solution with the objective function being a combination of real world limitations, like distances to obstacles, optimal angles and minimizing accelerations. 

Right now it tracks the cursor in two dimensions, but the code should be quite ready for 3D too. 

A substantial amount of code is just in the matrix library that I made for fun for this project. 

![](https://github.com/peik-etzell/inverse_kinematics_testing/blob/main/GIF.gif)
