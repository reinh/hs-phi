{-# LANGUAGE RecordWildCards #-}

-- | 'Phi' implements a
-- <http://paperhub.s3.amazonaws.com/f516fdfa940caa08c679d3946b273128.pdf φ accrual>
-- failure detector. It uses 'Double' as an abstract notion of time so
-- users will have to convert to 'Double' from some sort of clock
-- (hopefully a monotonic one).
module Phi
  ( Detector, mkDetector
  , ping
  , phi, φ
  ) where

import qualified Phi.Window as W

data Detector =
  Detector
  { window     :: !W.Window -- ^ The sliding window of latencies
  , minSamples :: !Int      -- ^ The minimum number of samples that
                            -- must be collected before the detector
                            -- can function
  , latest     :: !Double   -- ^ The latest recorded timestamp
  }

mkDetector :: Int      -- ^ The window size
           -> Int      -- ^ The minimum number of samples
           -> Double   -- ^ The latest ping
           -> Detector
mkDetector = Detector . W.mkWindow

-- | Add a ping to the detector.
ping :: Detector -> Double -> Detector
ping Detector{..} now =
  Detector (W.push (now - latest) window) minSamples now

-- | Calculate ϕ for the detector.
phi :: Detector -> Double -> Maybe Double
phi Detector{..} now
  | W.length window < minSamples = Nothing
  | otherwise = Just (W.phi window now)

-- | Calculate ϕ for the detector. Yay unicode!
φ :: Detector -> Double -> Maybe Double
φ = phi
