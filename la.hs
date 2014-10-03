import Data.Maybe

-- A line ax + by = c
-- Note that an equivalence relation would need
-- a normalizing of lines
data Line = Line Float Float Float deriving (Show)

-- A Point
data Point = Point Float Float deriving (Show, Eq)

-- A Circle p, radius
data Circle = Circle Point Float deriving (Show, Eq)

-- Calculates the middle of two points
middle_of :: Point -> Point -> Point
middle_of (Point x1 y1) (Point x2 y2) = (Point newX newY)
  where newX = (x1 + x2)/2
        newY = (y1 + y2)/2

-- Get a line through two points
line_from_to :: Point -> Point -> Maybe Line
line_from_to p1 p2 =
  if p1 == p2
     then Nothing
     else Just (Line a b c)
       where (Point x1 y1) = p1
             (Point x2 y2) = p2
             a = y2 - y1
             b = x1 - x2
             c = x1*y2 - x2*y1

-- Intersect two lines
intersect :: Line -> Line -> Maybe Point
intersect (Line a b s) (Line c d t) =
  if det == 0
     then Nothing
     else Just (Point x y)
       where det = a*d - b*c
             x = (d*s - b*t)/det
             y = (-c*s + a*t)/det

-- Get a orthogonal line
orthogonal_to_through :: Line -> Point -> Line
orthogonal_to_through (Line a b _) (Point x y) = (Line s t u)
  where s = (-1.0) * b
        t = a
        u = s*x + t*y

-- Get the distance of two points
distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2) =
  sqrt(x*x + y*y)
    where x = x2 - x1
          y = y2 - y1

-- Get a circle through 3 Points
describing_circle :: Point -> Point -> Point -> Maybe Circle
describing_circle p1 p2 p3 =
  case intersection of
       Nothing -> Nothing
       Just center -> case radius of
                      Nothing -> Nothing
                      Just d -> Just (Circle center d)
    where p1p2 = line_from_to p1 p2
          p2p3 = line_from_to p2 p3
          q1 = middle_of p1 p2
          q2 = middle_of p2 p3
          line1 = maybe Nothing (Just . (((flip orthogonal_to_through) q1))) p1p2
          line2 = maybe Nothing (Just . (((flip orthogonal_to_through) q2))) p2p3
          helper = maybe Nothing (Just . intersect) line1
          intersection = case helper of
                              Nothing -> Nothing
                              Just f -> maybe Nothing f line2
          radius = maybe Nothing (Just . (distance p1)) intersection
