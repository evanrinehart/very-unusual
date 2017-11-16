module R2 where

type R = Double
data R2 = R2 !R !R
  deriving (Show, Eq, Ord)

r2x (R2 x y) = x
r2y (R2 x y) = y

R2 x1 y1 ^+^ R2 x2 y2 = R2 (x1 + x2) (y1 + y2)
R2 x1 y1 ^-^ R2 x2 y2 = R2 (x1 - x2) (y1 - y2)
s *^ R2 x y = R2 (s * x) (s * y)
R2 x y ^* s = R2 (x * s) (y * s)
R2 x y ^/ s = R2 (x / s) (y / s)
dot (R2 x1 y1) (R2 x2 y2) = x1 * x2 + y1 * y2
cross (R2 x1 y1) (R2 x2 y2) = x1 * y2 - x2 * y1
negR2 (R2 x y) = R2 (-x) (-y)
rotR2 theta (R2 x y) = R2 (x * cos theta - y * sin theta) (y * cos theta + x * sin theta)
norm (R2 x y) = sqrt (x*x + y*y)

normalize r = r ^/ norm r

type Triangle = (R2,R2,R2)

inside :: Triangle -> R2 -> Bool
inside (p1,p2,p3) x = answer where
  v1 = p2 ^-^ p1 
  v2 = p3 ^-^ p2
  v3 = p1 ^-^ p3
  in1 = (x ^-^ p1) `cross` v1 < 0
  in2 = (x ^-^ p2) `cross` v2 < 0
  in3 = (x ^-^ p3) `cross` v3 < 0
  answer = in1 && in2 && in3


lerpR2 :: R -> R2 -> R2 -> R2
lerpR2 dt x v = x ^+^ v ^* dt

lerpRot :: R -> R2 -> R -> R2
lerpRot dt theta omega = rotR2 (omega * dt) theta
