{-# LANGUAGE InstanceSigs #-}

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

-- | Simplex algorithm result is of this type
data SimplexResult
  = FeasibleUnbounded
  | Infeasible
  | Optimal Tableau (Vector Rational)

instance Show SimplexResult where
  show :: SimplexResult -> String
  show FeasibleUnbounded = "Feasible Unbound"
  show Infeasible = "Infeasible"
  show (Optimal tab sol) =
    "Solution Vector: "
      ++ show sol
      ++ "\n"
      ++ show tab

------------------------------------------------------------------------------------------

-- | Simplex algorithm state, basicColsIndices are indexed from 1
data SimplexState = SimplexState
  { tableau :: Tableau,
    -- size basicColsIndices = nrows tableau - 1
    -- basicColsIndices ! i = index of the column of the respective basic variable
    basicColsIndices :: Vector Int
  }

instance Show SimplexState where
  show :: SimplexState -> String
  show (SimplexState tab indices) =
    "Basic Column Indices: "
      ++ show indices
      ++ "\n"
      ++ show tab