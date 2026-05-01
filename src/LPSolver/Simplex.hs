module LPSolver.Simplex where

import Data.Matrix (Matrix, fromList, fromLists, identity, nrows, (<->), (<|>))
import Data.Ratio ((%))
import LPSolver.Types (LPInstance (..))

inst :: LPInstance
inst =
  LPInstance
    { matA =
        [ [1, 1, 3],
          [2, 2, 5],
          [4, 1, 2]
        ],
      vecB = [30, 24, 36],
      vecC = [-3, -1, -2]
    }

instanceMatrix :: [[Integer]]
instanceMatrix =
  [ [1, 1, 3, 30],
    [2, 2, 5, 24],
    [4, 1, 2, 36],
    [-3, -1, -2, 0]
  ]

-- | Combines matA, vecB, vecC and an identity matrix to build the tableau
buildTableau :: LPInstance -> Matrix Rational
buildTableau (LPInstance a b c) =
  (% 1) <$> (aWithC <|> identityMat) <|> bWithZero
  where
    aWithC = fromLists a <-> fromList 1 (length c) c
    identityMat = identity (nrows aWithC)
    bWithZero = fromList (length b + 1) 1 (b ++ [0])