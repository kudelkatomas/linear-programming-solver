{-# LANGUAGE InstanceSigs #-}

module LPSolver.Types where

import Control.Monad.Error.Class (MonadError (catchError))
import Data.Matrix (Matrix (..), getCol, getRow)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.ParserCombinators.Parsec (ParseError)

------------------------------------------------------------------------------------------
-- LPInstance

-- | Linear programming problem instance
data LPInstance = LPInstance
  { matA :: [[Integer]],
    vecB :: [Integer],
    vecC :: [Integer]
  }

instance Show LPInstance where
  show :: LPInstance -> String
  show (LPInstance a b c) =
    "LP Instance: A = "
      ++ show a
      ++ "; b = "
      ++ show b
      ++ "; c = "
      ++ show c

------------------------------------------------------------------------------------------
-- Tableau

-- | Simplex algorithm tableau
type Tableau = Matrix Rational

getLastCol :: Tableau -> Vector Rational
getLastCol tbl = getCol (ncols tbl) tbl

getLastRow :: Tableau -> Vector Rational
getLastRow tbl = getRow (nrows tbl) tbl

------------------------------------------------------------------------------------------
-- SimplexStatus

-- | Represents the status of the computation at a specific phase
data SimplexStatus
  = Running
  | Infeasible
  | Unbounded
  | Optimal
  deriving (Show)

------------------------------------------------------------------------------------------
-- SimplexState

-- | Simplex algorithm state, basicColsIndices are indexed from 1.
data SimplexState = SimplexState
  { tableau :: Tableau,
    -- size basicColsIndices = nrows tableau - 1
    -- basicColsIndices ! i = index of the column of the respective basic variable
    basicColsIndices :: Vector Int,
    status :: SimplexStatus
  }

instance Show SimplexState where
  show :: SimplexState -> String
  show (SimplexState tab indices stat) =
    "Status: "
      ++ show stat
      ++ "\n"
      ++ "Basic Column Indices: "
      ++ show indices
      ++ "\n"
      ++ show tab

------------------------------------------------------------------------------------------
-- SimplexM

data SimplexM a = State SimplexState a

------------------------------------------------------------------------------------------
-- Result printing

printCompactResult :: SimplexState -> String
printCompactResult state = case status state of
  Running -> "Computation is in progress"
  Infeasible -> "Infeasible"
  Unbounded -> "Feasible Unbounded"
  Optimal -> "Solution Vector: " ++ show (getSolutionVector state)

printResult :: SimplexState -> String
printResult state = printCompactResult state ++ "\n" ++ show state

-- | Extracts solution vector [x_1, ..., x_n, objective function value] from tableau.
getSolutionVector :: SimplexState -> V.Vector Rational
getSolutionVector (SimplexState tab basicColsIdxs _) =
  V.fromList $ fmap step [1 .. ncols tab - 1] ++ [V.last lastCol]
  where
    lastCol = getLastCol tab

    step :: Int -> Rational
    step i = maybe 0 (lastCol V.!) (V.elemIndex i basicColsIdxs)

{-
-- | Extracts tableau and solution vector from simplex state,
--   it is expected that optimal solution was found.
getOptimalSolution :: SimplexState -> SimplexResult
getOptimalSolution state = Optimal state $ getSolutionVector state
-}

------------------------------------------------------------------------------------------
-- Error handling
-- inspired by https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

data SimplexError
  = Parser ParseError
  | NotImplemented String
  | LogicError String
  deriving (Show)

-- type ThrowsError a = Either SimplexError a
type ThrowsError = Either SimplexError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

-- | Extracts the string or converts the error to a string.
safeExtractString :: ThrowsError String -> String
safeExtractString = extractValue . trapError
  where
    -- https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
    -- "We purposely leave extractValue undefined for a Left constructor,
    -- because that represents a programmer error."
    extractValue :: ThrowsError b -> b
    extractValue (Right val) = val
    extractValue (Left _) = undefined

-- | Extracts the value as a string using Show or converts the error to string.
safeShowValue :: (Show a) => ThrowsError a -> String
safeShowValue = safeExtractString . fmap show

safeShowCompactValue :: ThrowsError SimplexState -> String
safeShowCompactValue = safeExtractString . fmap printCompactResult
