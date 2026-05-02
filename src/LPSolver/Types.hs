module LPSolver.Types where

import Data.Matrix (Matrix (..), getCol, getRow)
import Data.Vector (Vector)

------------------------------------------------------------------------------------------

-- | Linear programming problem instance
data LPInstance = LPInstance
  { matA :: [[Integer]],
    vecB :: [Integer],
    vecC :: [Integer]
  }

------------------------------------------------------------------------------------------

-- | Simplex algorithm tableau
type Tableau = Matrix Rational

getLastCol :: Tableau -> Vector Rational
getLastCol tbl = getCol (ncols tbl) tbl

getLastRow :: Tableau -> Vector Rational
getLastRow tbl = getRow (nrows tbl) tbl

------------------------------------------------------------------------------------------