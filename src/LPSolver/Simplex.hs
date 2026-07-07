{-# LANGUAGE FlexibleContexts #-}

module LPSolver.Simplex (simplex) where

import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (MonadState, StateT (..), get, modify', put)
import Data.Matrix
  ( Matrix (ncols),
    colVector,
    combineRows,
    fromLists,
    getCol,
    getElem,
    identity,
    nrows,
    rowVector,
    scaleRow,
    submatrix,
    (<->),
    (<|>),
  )
import Data.Maybe (isJust)
import Data.Ratio ((%))
import qualified Data.Vector as V
  ( Vector,
    elemIndex,
    findIndex,
    fromList,
    indexed,
    init,
    maxIndex,
    minIndex,
    zip,
    zipWith,
    (!),
    (//),
  )
import LPSolver.Types
  ( LPInstance (..),
    SimplexError (..),
    SimplexM (runSimplexM),
    SimplexState (..),
    SimplexStatus (..),
    Tableau,
    ThrowsError,
    getLastCol,
    getLastRow,
    getObjectiveFunctionValue,
  )

------------------------------------------------------------------------------------------
-- Instances for testing purposes

testInst :: LPInstance
testInst =
  LPInstance
    { matA =
        [ [1, 1, 3],
          [2, 2, 5],
          [4, 1, 2]
        ],
      vecB = [30, 24, 36],
      vecC = [3, 1, 2]
    }

testInst2 :: LPInstance
testInst2 =
  LPInstance
    { matA =
        [ [2, -1],
          [1, -5]
        ],
      vecB = [2, -4],
      vecC = [2, -1]
    }

------------------------------------------------------------------------------------------
-- Initialize SimplexState

-- | Combines matA, vecB, vecC and an identity matrix into a tableau and
--   initializes basic columns indices.
initializeSimplexState :: LPInstance -> SimplexState
initializeSimplexState (LPInstance a b c) =
  SimplexState
    { tableau = (% 1) <$> (aIdentityC <|> bWithZero),
      basicColsIndices = basicColsIndices',
      status = Running
    }
  where
    aMat = fromLists a
    aNRows = nrows aMat
    aNCols = ncols aMat
    cWithZeros = fmap negate c ++ replicate aNRows 0
    bWithZero = (colVector . V.fromList) (b ++ [0])
    aIdentityC =
      (aMat <|> identity aNRows) <-> (rowVector . V.fromList) cWithZeros
    basicColsIndices' = V.fromList [aNCols + i | i <- [1 .. aNRows]]

-- | Initializes initial state for initSimplex algorithm
initializeInitSimplexState :: LPInstance -> SimplexState
initializeInitSimplexState ins@(LPInstance a _ c) =
  -- vecC = -1 : ..., where -1 because initializeSimplexState negates vecC
  initializeSimplexState (ins {matA = fmap (-1 :) a, vecC = -1 : fmap (const 0) c})

testInstInitState :: SimplexState
testInstInitState = initializeSimplexState testInst

{-
>>> testInstInitState
>>> initializeInitSimplexState testInst
Result: Computation is in progress
Basic Column Indices: [4,5,6]
┌                                                                ┐
│    1 % 1    1 % 1    3 % 1    1 % 1    0 % 1    0 % 1   30 % 1 │
│    2 % 1    2 % 1    5 % 1    0 % 1    1 % 1    0 % 1   24 % 1 │
│    4 % 1    1 % 1    2 % 1    0 % 1    0 % 1    1 % 1   36 % 1 │
│ (-3) % 1 (-1) % 1 (-2) % 1    0 % 1    0 % 1    0 % 1    0 % 1 │
└                                                                ┘
Result: Computation is in progress
Basic Column Indices: [5,6,7]
┌                                                                         ┐
│ (-1) % 1    1 % 1    1 % 1    3 % 1    1 % 1    0 % 1    0 % 1   30 % 1 │
│ (-1) % 1    2 % 1    2 % 1    5 % 1    0 % 1    1 % 1    0 % 1   24 % 1 │
│ (-1) % 1    4 % 1    1 % 1    2 % 1    0 % 1    0 % 1    1 % 1   36 % 1 │
│    1 % 1    0 % 1    0 % 1    0 % 1    0 % 1    0 % 1    0 % 1    0 % 1 │
└                                                                         ┘
-}

------------------------------------------------------------------------------------------
-- Find pivot

-- | Finds pivot's column index following the Bland's rule or
--   Nothing if every c_j \<= 0 (i.e. -c_j \>=0).
findPivotColumnIndex :: Tableau -> Maybe Int
findPivotColumnIndex = fmap (+ 1) . V.findIndex (< 0) . V.init . getLastRow

-- | Finds pivot's row index following the Bland's rule or
--   Nothing if a_{column, row_i} \<= 0 for every i.
findPivotRowIndex :: SimplexState -> Int -> Maybe Int
findPivotRowIndex (SimplexState tab basicColsIdxs _) pivotColIdx =
  fst (ratios V.! minIdx) >> Just (minIdx + 1)
  where
    -- Drop the last row
    b = V.init $ getLastCol tab
    col = V.init $ getCol pivotColIdx tab

    -- Nothing < Just _, therefore ratios multiplied by -1 and V.maxIndex.
    -- zip with basicColsIndices (negate because of V.maxIndex) to follow Bland's rule.
    ratios = V.zip (V.zipWith safeNegRatio b col) (fmap negate basicColsIdxs)
    minIdx = V.maxIndex ratios

    -- Dividing by -d to multiply the ratio by -1, see the comment above.
    safeNegRatio :: Rational -> Rational -> Maybe Rational
    safeNegRatio n d = if d > 0 then Just (n / (-d)) else Nothing

findPivotTest :: Maybe (Int, Int)
findPivotTest = do
  col <- findPivotColumnIndex (tableau testInstInitState)
  row <- findPivotRowIndex testInstInitState col
  return (col, row)

{-
>>> findPivotTest
Just (1,3)
-}

------------------------------------------------------------------------------------------
-- Pivoting

-- | Given pivot's coordinates (column, row), tableau and target row,
--   performs one row update.
updateTableauRow :: (Int, Int) -> Tableau -> Int -> Tableau
updateTableauRow (col, row) tab targetRow =
  combineRows targetRow (-(getElem targetRow col tab)) row tab

-- | Given tableau and pivot's coordinates (column, row),
--   performs one tableau update.
updateTableau :: Tableau -> (Int, Int) -> Tableau
updateTableau tab coords@(col, row) =
  let targetRows = [i | i <- [1 .. nrows tab], i /= row]
      pivot = getElem row col tab
      scaledRowTab = scaleRow (1 / pivot) row tab
   in foldl (updateTableauRow coords) scaledRowTab targetRows

-- | Given pivot's coordinates (column, row),
--   performs pivoting, updating the state accordingly.
pivotAt :: (Int, Int) -> SimplexState -> SimplexState
pivotAt coords@(col, row) state@(SimplexState tab basicColsIdxs _) =
  state
    { tableau = updateTableau tab coords,
      basicColsIndices = basicColsIdxs V.// [(row - 1, col)]
    }

pivotAtTest :: Maybe SimplexState
pivotAtTest = do
  pivotCol <- findPivotColumnIndex (tableau testInstInitState)
  pivotRow <- findPivotRowIndex testInstInitState pivotCol
  return $ pivotAt (pivotCol, pivotRow) testInstInitState

{-
>>> pivotAtTest
Just Status: Computation is in progress
Basic Column Indices: [4,5,1]
┌                                                                ┐
│    0 % 1    3 % 4    5 % 2    1 % 1    0 % 1 (-1) % 4   21 % 1 │
│    0 % 1    3 % 2    4 % 1    0 % 1    1 % 1 (-1) % 2    6 % 1 │
│    1 % 1    1 % 4    1 % 2    0 % 1    0 % 1    1 % 4    9 % 1 │
│    0 % 1 (-1) % 4 (-1) % 2    0 % 1    0 % 1    3 % 4   27 % 1 │
└                                                                ┘
-}

-- | Implements the main part of the simplex algorithm,
--   i.e. finding the pivot and updating the tableau
--   until optimal solution is found.
pivotLoop :: (MonadState SimplexState m) => m ()
pivotLoop = do
  state <- get
  case findPivotColumnIndex (tableau state) of
    -- Nothing => Optimal solution was found
    Nothing -> put $ state {status = Optimal}
    Just pivotCol -> case findPivotRowIndex state pivotCol of
      -- Nothing => The instance is feasible unbounded
      Nothing -> put $ state {status = Unbounded}
      Just pivotRow -> do
        modify' $ pivotAt (pivotCol, pivotRow)
        pivotLoop

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
-- Find initial feasible solution

------------------------------------------------------------------------------------------
-- Transformation helpers

-- | Removes x_0 from tableau and updates basic columns indices.
removeX0 :: SimplexState -> SimplexState
removeX0 state@(SimplexState tab basicColsIdxs _) =
  state
    { tableau = submatrix 1 (nrows tab) 2 (ncols tab) tab,
      basicColsIndices = fmap (subtract 1) basicColsIdxs
    }

-- | Extends vector C to given size and negates it.
transformC :: [Integer] -> Int -> V.Vector Rational
transformC c size = V.fromList $ fmap ((% 1) . negate) c ++ replicate (size - length c) 0

-- | Replaces the last row of the tableau with given vector C.
--   Also transforms C appropriately.
replaceLastRow :: Tableau -> [Integer] -> Tableau
replaceLastRow tab c =
  let tabWithoutLastRow = submatrix 1 (nrows tab - 1) 1 (ncols tab) tab
   in tabWithoutLastRow <-> rowVector (transformC c (ncols tab))

updateLastRowStep :: Int -> Tableau -> (Int, Int) -> Tableau
updateLastRowStep lastRowIdx tab (rowIdx, basicColIdx) =
  let lastComponent = getElem lastRowIdx basicColIdx tab
   in if lastComponent /= 0
        -- rowIdx + 1 because V.indexed indexes from 0 and tableau indexes from 1
        then combineRows lastRowIdx (-lastComponent) (rowIdx + 1) tab
        else tab

-- | Updates given vector C so that all basic columns have zero
--   as their last component and calculates the value
--   of the objective function.
--   Replaces last row of the tableau with the result.
updateLastRow :: [Integer] -> SimplexState -> SimplexState
updateLastRow c state@(SimplexState tab basicColsIdxs _) =
  state {tableau = foldl step updatedTab (V.indexed basicColsIdxs)}
  where
    updatedTab = replaceLastRow tab c
    step = updateLastRowStep (nrows updatedTab)

-- | Helper for safeTransformAndSolve. Removes x_0 column from tableau,
--   updates basic columns indices, replaces tableau's last row
--   and marks the instance as running.
transformInitSimplexInstance :: (MonadState SimplexState m) => [Integer] -> m ()
transformInitSimplexInstance c = do
  modify' removeX0
  modify' $ updateLastRow c
  modify' (\s -> s {status = Running})

------------------------------------------------------------------------------------------

-- | If the objective function value is non-zero, marks the instance as infeasible.
--   Otherwise, transforms the state accordingly and solves the instance.
--   Throws an error if x_0 is basic, since that case is not implemented yet.
safeTransformAndSolve ::
  (MonadError SimplexError m, MonadState SimplexState m) => [Integer] -> m ()
safeTransformAndSolve c = do
  state@(SimplexState tab basicColsIdxs _) <- get

  if getObjectiveFunctionValue tab /= 0
    then put $ state {status = Infeasible}
    else
      -- Throws an error if x_0 is basic, since that case is not implemented yet.
      if isJust $ V.elemIndex 1 basicColsIdxs
        then
          throwError
            ( NotImplemented
                ( "InitSimplex found initial feasible solution, but x_0 is basic.\
                  \ This case is not implemented yet.\n"
                    ++ show tab
                    ++ "\n"
                )
            )
        -- Remove x_0 from tableau, update basic columns indices and solve the result.
        else do
          transformInitSimplexInstance c
          -- Solve the transformed instance.
          pivotLoop

-- | Tries to solve the instance if initial feasible solution was found.
--   Or marks the instance as infeasible if it is infeasible.
--   Otherwise, throws appropriate error.
safeEvaluateAndSolve ::
  (MonadError SimplexError m, MonadState SimplexState m) => [Integer] -> m ()
safeEvaluateAndSolve c = do
  state <- get
  case status state of
    Optimal -> safeTransformAndSolve c
    Unbounded ->
      throwError
        ( NotImplemented
            "InitSimplex initial instance is unbounded. This case is not implemented yet."
        )
    _ ->
      throwError
        ( LogicError
            ( "InitSimplex's initial instance is neither optimal\
              \ nor unbounded. This should never happen."
                ++ show state
            )
        )

-- | Implements initSimplex algorithm and solves its result if possible.
--   Finds initial feasible solution if it exists and solves the instance.
--   Or marks the instance as infeasible if it is infeasible.
--   Otherwise, throws appropriate error.
initSimplexAndSolve ::
  (MonadError SimplexError m, MonadState SimplexState m) => [Integer] -> m ()
initSimplexAndSolve c = do
  (SimplexState tab basicColsIdxs _) <- get

  let pivotCol = 1
      -- zip with basicColsIndices to follow Bland's rule
      pivotRow = 1 + V.minIndex (V.zip ((V.init . getLastCol) tab) basicColsIdxs)

  -- "all (>= 0) b" holds in this state
  modify' $ pivotAt (pivotCol, pivotRow)
  -- solve the initSimplex instance
  pivotLoop
  -- evaluate the result and solve it if possible
  safeEvaluateAndSolve c

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
-- Simplex algorithm

simplex :: (Monad m) => LPInstance -> m (ThrowsError (), SimplexState)
simplex ins@(LPInstance _ b c) =
  let (stateInitializer, solver) =
        if all (>= 0) b
          then (initializeSimplexState, pivotLoop)
          else (initializeInitSimplexState, initSimplexAndSolve c)
   in runStateT (runExceptT (runSimplexM solver)) (stateInitializer ins)

{-
>>> simplex testInst
>>> simplex testInst2
(Right (),Result: Solution Vector: [8 % 1,4 % 1,0 % 1,18 % 1,0 % 1,0 % 1,28 % 1]
Basic Column Indices: [4,2,1]
┌                                                                ┐
│    0 % 1    0 % 1    1 % 2    1 % 1 (-1) % 2    0 % 1   18 % 1 │
│    0 % 1    1 % 1    8 % 3    0 % 1    2 % 3 (-1) % 3    4 % 1 │
│    1 % 1    0 % 1 (-1) % 6    0 % 1 (-1) % 6    1 % 3    8 % 1 │
│    0 % 1    0 % 1    1 % 6    0 % 1    1 % 6    2 % 3   28 % 1 │
└                                                                ┘)
(Right (),Result: Solution Vector: [14 % 9,10 % 9,0 % 1,0 % 1,2 % 1]
Basic Column Indices: [1,2]
┌                                              ┐
│    1 % 1    0 % 1    5 % 9 (-1) % 9   14 % 9 │
│    0 % 1    1 % 1    1 % 9 (-2) % 9   10 % 9 │
│    0 % 1    0 % 1    1 % 1    0 % 1    2 % 1 │
└                                              ┘)
-}
