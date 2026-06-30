{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module LPSolver.Types where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.State.Strict (MonadState, StateT)
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
  show state =
    "Result: "
      ++ showCompact state
      ++ "\n"
      ++ "Basic Column Indices: "
      ++ show (basicColsIndices state)
      ++ "\n"
      ++ show (tableau state)

------------------------------------------------------------------------------------------
-- Result printing

showCompact :: SimplexState -> String
showCompact state = case status state of
  Running -> "Computation is in progress"
  Infeasible -> "Infeasible"
  Unbounded -> "Feasible Unbounded"
  Optimal -> "Solution Vector: " ++ show (getSolutionVector state)

-- | Extracts objective function value from tableau.
getObjectiveFunctionValue :: Tableau -> Rational
getObjectiveFunctionValue tab = V.last $ getLastCol tab

-- | Extracts solution vector [x_1, ..., x_n, objective function value] from tableau.
getSolutionVector :: SimplexState -> V.Vector Rational
getSolutionVector (SimplexState tab basicColsIdxs _) =
  V.fromList $
    fmap (step (getLastCol tab)) [1 .. ncols tab - 1]
      ++ [getObjectiveFunctionValue tab]
  where
    step :: V.Vector Rational -> Int -> Rational
    step lastCol i = maybe 0 (lastCol V.!) (V.elemIndex i basicColsIdxs)

{-
-- | Extracts tableau and solution vector from simplex state,
--   it is expected that optimal solution was found.
getOptimalSolution :: SimplexState -> SimplexResult
getOptimalSolution state = Optimal state $ getSolutionVector state
-}

------------------------------------------------------------------------------------------
-- Error handling

data SimplexError
  = Parser ParseError
  | NotImplemented String
  | LogicError String
  deriving (Show)

type ThrowsError a = Either SimplexError a

-- | Extracts the string or converts the error to a string.
safeExtractString :: ThrowsError String -> String
safeExtractString = either show id

-- | Extracts the value as a string using show or converts the error to string.
safeShowValue :: (Show a) => ThrowsError a -> String
safeShowValue = safeExtractString . fmap show

-- | Extracts the value as a string using showCompact or converts the error to string.
safeShowCompactValue :: ThrowsError SimplexState -> String
safeShowCompactValue = safeExtractString . fmap showCompact

------------------------------------------------------------------------------------------
-- SimplexM

newtype SimplexM m a
  = SimplexM {runSimplexM :: ExceptT SimplexError (StateT SimplexState m) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadState SimplexState,
      MonadError SimplexError
    )