module World where

import R2
import Dir

data Samplers = Samplers
  { highScores :: [(String, Integer)]
  , joy1       :: (Int,Int)
  , joy2       :: (Int,Int) }
    deriving Show

data InputsRecord = InputsRecord
  { inFire1 :: Maybe ()
  , inFire2 :: Maybe ()
  , inJoy1  :: Maybe [(Dir, PR)]
  , inJoy2  :: Maybe [(Dir, PR)]
  , inCoin  :: Maybe [()]
  , inPlay1 :: Maybe ()
  , inPlay2 :: Maybe () }
      deriving Show

noExtEvents = InputsRecord
  { inFire1 = Nothing
  , inFire2 = Nothing 
  , inJoy1  = Nothing 
  , inJoy2  = Nothing
  , inCoin  = Nothing
  , inPlay1 = Nothing
  , inPlay2 = Nothing }

addFire1 :: InputsRecord -> InputsRecord
addFire1 r = r { inFire1 = Just () }

addFire2 :: InputsRecord -> InputsRecord
addFire2 r = r { inFire2 = Just () }

addJoy1 :: Dir -> PR -> InputsRecord -> InputsRecord
addJoy1 dir pr r = r { inJoy1 = append (dir,pr) (inJoy1 r) }

addJoy2 :: Dir -> PR -> InputsRecord -> InputsRecord
addJoy2 dir pr r = r { inJoy2 = append (dir,pr) (inJoy2 r) }

addPlay1 :: InputsRecord -> InputsRecord
addPlay1 r = r { inPlay1 = Just () }

addPlay2 :: InputsRecord -> InputsRecord
addPlay2 r = r { inPlay2 = Just () }

addCoin :: InputsRecord -> InputsRecord
addCoin r = r { inCoin = append () (inCoin r) }

append :: a -> Maybe [a] -> Maybe [a]
append x Nothing = Just [x]
append x (Just xs) = Just (xs ++ [x])

data StateRecord = StateRecord
  { shipX :: R2
  , shipV :: R2
  , shipTheta :: R2
  , credits :: Integer }
      deriving Show

data Emitters = Emitters
  { newHighScore :: World -> Maybe (String, Integer)
  }

data Timers = Timers
  { bulletTimeout :: World -> ETA Int }

data Equations = Equations
  { shipX' :: World -> R2
  , shipV' :: World -> R2
  , shipTheta' :: World -> R
  , force :: World -> R2
  , throttle :: World -> R
  , omega :: World -> R }

equations :: Equations
equations = Equations
  { shipX' = \w -> shipV (wState w)
  , shipV' = \w -> force (wEqns w) w
  , shipTheta' = \w -> omega (wEqns w) w
  , force = \w -> (throttle (wEqns w)) w *^ (shipTheta (wState w))
  , throttle = \w -> 0 -- "button state"
  , omega = \w -> 0 -- "joy state"
  }

data World = World
  { wState   :: StateRecord
  , wEqns    :: Equations
  , wSamps   :: Samplers
  , wEmitters :: Emitters
  , wTimers  :: Timers
  , wInputs  :: InputsRecord
  }

type Delta = Integer

data ETA a = InExactly Delta a | NotBefore Delta | Never
  deriving Show

integrate :: World -> Delta -> StateRecord -> StateRecord
integrate w nanos st =
  let eqns = wEqns w in
  let dt = fromIntegral nanos / (10^9) in
  st { shipX = lerpR2 dt (shipX st) (shipX' eqns w)
     , shipV = lerpR2 dt (shipV st) (shipV' eqns w)
     , shipTheta = normalize $ lerpRot dt (shipTheta st) (shipTheta' eqns w)
     }

onTimeout :: ETA a -> r -> (a -> r) -> r
onTimeout (InExactly 0 x) _ f = f x
onTimeout _               d _ = d

elapse :: Delta -> World -> World
elapse nanos w = w { wState = integrate w nanos (wState w) }

