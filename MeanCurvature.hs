module MeanCurvature where

import Data.Maybe()
import LA

data Curve = Curve [Point] deriving (Eq, Show)

-- Move Point in direction of mean curvature
move :: Point -> Maybe Circle -> Point
move point circle =
  case circle of
       Nothing -> point
       (Just (Circle (Point a b) r)) ->
         let factor = speed*time/r
             newX = x + (a - x)*factor
             newY = y + (b - y)*factor
             in Point newX newY
    where (Point x y) = point
          speed = 0.01
          time = 1.0


-- Iterate one step further
step :: Curve -> Curve
step (Curve xs) =
  Curve $ zipWith move xs mean_curvatures
    where list = [last xs] ++ xs ++ [head xs]
          a = tail $ tail list
          b = init $ tail list
          c = init $ init list
          helper = zipWith describingCircle a b
          mean_curvatures = zipWith ($) helper c
