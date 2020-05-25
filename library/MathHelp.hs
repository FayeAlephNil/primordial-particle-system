-- | Some Helpful Math For Us

module MathHelp where

import Defs

norm2 :: Position -> Float
norm2 (x, y) = x ** 2 + y ** 2

norm :: Position -> Float
norm = sqrt . norm2

distance2 :: Position -> Position -> Float
distance2 (x1, y1) (x2, y2) = norm2 (x1 - x2, y1 - y2)

polarToCart :: (Float, Float) -> (Float, Float)
polarToCart (mag, theta) = (mag * cos theta, mag * sin theta)

cartToPolar :: (Float, Float) -> (Float, Float)
cartToPolar (x, y) = (norm (x, y), atan2 y x)

sgn :: Int -> Int
sgn 0 = 0
sgn x = if x > 0 then 1 else -1

