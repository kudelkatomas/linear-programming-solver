module LPSolver.Types where

-- | Linear programming problem instance
data LPInstance = LPInstance
  { matA :: [[Integer]],
    vecB :: [Integer],
    vecC :: [Integer]
  }
