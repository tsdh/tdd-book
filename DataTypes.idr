data Direction = North | East | South | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle x y) = x * y / 2
area (Rectangle x y) = x * y
area (Circle r) = pi * r * r

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

%name Picture pic, pic1, pic2

testPicture : Picture
testPicture = Combine (Translate 5 5 (Primitive (Rectangle 20 10)))
                      (Combine (Translate 35 5 (Primitive (Circle 5)))
                               (Translate 15 25 (Primitive (Triangle 10 10))))
                               
pictureArea : Picture -> Double
pictureArea (Primitive p) = area p
pictureArea (Combine p1 p2) = (pictureArea p1) + (pictureArea p2)
pictureArea (Rotate _ p) = pictureArea p
pictureArea (Translate x y p) = pictureArea p

areaOfBiggestTriangle : Picture -> Maybe Double
areaOfBiggestTriangle (Primitive t@(Triangle x y)) = Just $ area t
areaOfBiggestTriangle (Primitive (Rectangle x y)) = Nothing
areaOfBiggestTriangle (Primitive (Circle x)) = Nothing
areaOfBiggestTriangle (Combine pic1 pic2) = let a1 = areaOfBiggestTriangle pic1
                                                a2 = areaOfBiggestTriangle pic2 in
                                            (case a1 of
                                              Nothing  => a2
                                              (Just a) => (case a2 of
                                                             Nothing  => a1
                                                             (Just b) => if a <= b then a2 else a1))
areaOfBiggestTriangle (Rotate d pic) = areaOfBiggestTriangle pic
areaOfBiggestTriangle (Translate x y pic) = areaOfBiggestTriangle pic


