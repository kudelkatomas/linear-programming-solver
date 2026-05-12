{-# LANGUAGE InstanceSigs #-}

module LPSolver.Types where

import Control.Monad.Error.Class (MonadError (catchError))
import Data.Matrix (Matrix (..), getCol, getRow)
import Data.Vector (Vector)
import Text.ParserCombinators.Parsec (ParseError)

------------------------------------------------------------------------------------------

-- | Linear programming problem instance
data LPInstance = LPInstance
  { matA :: [[Integer]],
    vecB :: [Integer],
    vecC :: [Integer]
  }
  deriving (Show)

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

-- | Simplex algorithm state, basicColsIndices are indexed from 1.
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

------------------------------------------------------------------------------------------

-- Error handling is inspired by
-- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

newtype SimplexError = Parser ParseError deriving (Show)

-- type ThrowsError a = Either SimplexError a
type ThrowsError = Either SimplexError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

-- "We purposely leave extractValue undefined for a Left constructor,
-- because that represents a programmer error."
extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left _) = undefined

-- | Extracts the value or converts the error to string.
safeExtractValue :: Either SimplexError String -> String
safeExtractValue = extractValue . trapError