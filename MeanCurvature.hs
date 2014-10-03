module MeanCurvature where

import LA

data Curve = Curve [Point]

-- Move Point in direction of mean curvature
move :: Point -> Circle -> Point
move (Point x y) (Circle (Point a b) r) =
  Point newX newY
    where factor = speed*time/r
          newX = x + (a - x)*factor
          newY = y + (b - y)*factor
          speed = 0.01
          time = 1.0


-- Iterate one step further
iterate :: Curve -> Curve
iterate (Curve xs) =
  Curve $ zipWith move xs mean_curvatures
    where list = xs ++ (tail xs)
          a = tail $ tail list
          b = init $ tail list
          c = init $ init list
          helper = zipWith describingCircle a b
          mean_curvatures = zipWith (\(f, x) -> (f x)) helper c
