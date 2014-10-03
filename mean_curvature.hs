module MeanCurvature where

import la

data Curve Point = Curve [Point]

let speed :: float = 0.01
let time :: float = 1.0

-- Move Point in direction of mean curvature
move :: Point -> Circle -> Point
move (Point x y) (Circle (Point a b) r) =
  Point newX newY
    where factor = speed*time/r
          newX = x + (a - x)*factor
          newY = y + (b - y)*factor


-- Iterate one step further
iterate :: Curve -> Curve
iterate Curve xs =
  Curve $ zipWith move xs mean_curvatures
    where list =
