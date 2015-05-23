{-# LANGUAGE RecordWildCards #-}
module Phi.Window (Window, length, mkWindow, push, phi) where

import           Data.Vector.Unboxed            (Vector)
import qualified Data.Vector.Unboxed            as V
import           Prelude                        hiding (length)
import           Statistics.Distribution        (complCumulative)
import           Statistics.Distribution.Normal (normalFromSample)

data Window =
  Window
  { sample :: !(Vector Double)
  , size   :: !Int
  }

mkWindow :: Int -> Window
mkWindow = Window V.empty

push :: Double -> Window -> Window
push d window = window {sample = sample'}
  where sample' =
          V.take (size window)
                 (V.cons d (sample window))

pLater :: Window -> Double -> Double
pLater =
  complCumulative . normalFromSample . sample

phi :: Window -> Double -> Double
phi w = negate . logBase 10 . pLater w

length :: Window -> Int
length = V.length . sample
