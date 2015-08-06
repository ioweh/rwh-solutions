data Point = Point Double Double deriving (Show, Eq)
data Direction = LeftTurn | RightTurn | Straight deriving (Show, Eq)

calculateTurn :: Point -> Point -> Point -> Direction
calculateTurn (Point x1 y1) (Point x2 y2) (Point x3 y3)
        | crossProduct > 0 = LeftTurn
        | crossProduct < 0 = RightTurn
        | otherwise        = Straight
            where crossProduct = (x2 - x1) * (y3 - y1) - (y2 - y1) * (x3 - x1)

isLeftTurn :: Point -> Point -> Point -> Bool
isLeftTurn p1 p2 p3 = calculateTurn p1 p2 p3 /= RightTurn

allTurnsAreLeft :: [Point] -> (Bool, [Point])
allTurnsAreLeft [p0,p1,p2] =			-- if we have only 3 points left
    if (isLeftTurn p0 p1 p2)
        then (True, [p0,p1,p2])
        else (False, [p0,p2])
allTurnsAreLeft (p0:p1:p2:pts) = 
    if (isLeftTurn p0 p1 p2)
        then if (fst restOfThePoints)
                 then (True, p0:snd restOfThePoints)
                 else (False, snd (allTurnsAreLeft (p0:snd restOfThePoints)))
        else (False,(p0:p2:pts))
            where restOfThePoints = allTurnsAreLeft (p1:p2:pts)

grahamHull pts = snd . allTurnsAreLeft $ pts ++ [head pts]

points = [Point 0 0,Point 5 3,Point 4 5,Point 3 5,Point 3 11]

