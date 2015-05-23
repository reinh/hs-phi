{-# LANGUAGE RecordWildCards #-}
module Phi (Detector, mkDetector, ping, phi, ϕ) where

import qualified Phi.Window as W

data Detector =
  Detector
  { window     :: !W.Window -- ^ The sliding window of latency samples
  , minSamples :: !Int      -- ^ The minimum number of samples that
                            -- must be collected before the detector
                            -- can function
  , latest     :: !Double   -- ^ The latest recorded latency
  }

-- | Create a detector
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
ϕ :: Detector -> Double -> Maybe Double
ϕ = phi
