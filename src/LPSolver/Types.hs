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

getLastColumn :: Tableau -> Vector Rational
getLastColumn tbl = getCol (ncols tbl) tbl

getLastRow :: Tableau -> Vector Rational
getLastRow tbl = getRow (nrows tbl) tbl

------------------------------------------------------------------------------------------