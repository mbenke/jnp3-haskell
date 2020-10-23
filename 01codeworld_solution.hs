-- Flag
program  = drawingOf(redWheel & border)
redWheel = colored(wheel, red)
wheel    = solidCircle(4)
border   = rectangle(27, 12)

-- Traffic lights
program = drawingOf(trafficLights)
trafficLights = lamps & solidRectangle(5, 15)
lamps = translated(colored(lamp, green), 0, -5) &
        translated(colored(lamp, yellow), 0, 0) &
        translated(colored(lamp, red), 0, 5)
lamp = solidCircle(2)

-- Checkerboard
program = drawingOf(grid)
grid    = pictures([ if even(x+y) then translated(solidRectangle(1,1), x, y) else translated(rectangle(1,1), x, y)
                     | x <- [0..8], y <- [0..8] ])

-- Atom
program = drawingOf(atom)
atom = solidCircle(0.5) & pictures([rotated(ellipse, a) | a <- [45, 90, 135]])
ellipse = scaled(circle(4), 1, 0.25)

-- Sierpinski
program = drawingOf(fractal(3))

fractal :: Number -> Picture
fractal(0) = square
fractal(n) = square & pictures([translated(part, x, y) | x <- [-5, 0, 5], y <- [-5, 0, 5]])
  where part = scaled(fractal(n - 1), 1/3, 1/3)

square = solidRectangle(5, 5)

-- Animation
program   = animationOf(bounce)

bounce(t) = place(6 * t, 3, remainder(6 * t, 6) > 3)
place(val, m, neg) = translated(solidCircle(1), 0, if neg then m - remainder(val, m) else remainder(val, m))
