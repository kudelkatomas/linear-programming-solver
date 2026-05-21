{-# LANGUAGE LambdaCase #-}

module LPSolver.Simplex where

import Control.Monad.Error.Class (MonadError (throwError))
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
import Data.Ratio ((%))
import qualified Data.Vector as V
  ( Vector,
    elemIndex,
    findIndex,
    fromList,
    indexed,
    init,
    last,
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
    SimplexResult (..),
    SimplexState (..),
    SimplexStatus (..),
    Tableau,
    ThrowsError,
    getLastCol,
    getLastRow,
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

-- | Combines matA, vecB, vecC and an identity matrix into a tableau and
--   initializes basic columns indices.
initSimplexState :: LPInstance -> SimplexState
initSimplexState (LPInstance a b c) =
  SimplexState
    { tableau = (% 1) <$> (aIdentityC <|> bWithZero),
      basicColsIndices = basicColsIndices'
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

{-
>>> initSimplexState testInst
Basic Column Indices: [4,5,6]
┌                                                                ┐
│    1 % 1    1 % 1    3 % 1    1 % 1    0 % 1    0 % 1   30 % 1 │
│    2 % 1    2 % 1    5 % 1    0 % 1    1 % 1    0 % 1   24 % 1 │
│    4 % 1    1 % 1    2 % 1    0 % 1    0 % 1    1 % 1   36 % 1 │
│ (-3) % 1 (-1) % 1 (-2) % 1    0 % 1    0 % 1    0 % 1    0 % 1 │
└                                                                ┘
-}

------------------------------------------------------------------------------------------

-- | Finds pivot's column index following the Bland's rule or
--   Nothing if every c_j \<= 0 (i.e. -c_j \>=0).
findPivotColumnIndex :: Tableau -> Maybe Int
findPivotColumnIndex = fmap (+ 1) . V.findIndex (< 0) . V.init . getLastRow

-- | Finds pivot's row index following the Bland's rule or
--   Nothing if a_{column, row_i} \<= 0 for every i.
findPivotRowIndex :: SimplexState -> Int -> Maybe Int
findPivotRowIndex (SimplexState tab basicColsIdxs) pivotColIdx =
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
findPivotTest =
  do
    let state = initSimplexState testInst
    col <- findPivotColumnIndex (tableau state)
    row <- findPivotRowIndex state col
    return (col, row)

{-
>>> findPivotTest
Just (1,3)
-}

------------------------------------------------------------------------------------------

-- | Given the pivot's coordinates (column, row) performs one tableau update.
updateSimplexState :: SimplexState -> (Int, Int) -> SimplexState
updateSimplexState (SimplexState tab basicColsIdxs) (col, row) =
  SimplexState
    { tableau = foldl updateRow scaledTab targetRows,
      basicColsIndices = basicColsIdxs V.// [(row - 1, col)]
    }
  where
    targetRows = [i | i <- [1 .. nrows tab], i /= row]
    scaledTab =
      let pivot = getElem row col tab
       in scaleRow (1 / pivot) row tab

    updateRow :: Tableau -> Int -> Tableau
    updateRow tab' rowIdx = combineRows rowIdx (-(getElem rowIdx col tab')) row tab'

updateSimplexStateTest :: Maybe SimplexState
updateSimplexStateTest =
  do
    let state@(SimplexState tab _) = initSimplexState testInst
    pivotCol <- findPivotColumnIndex tab
    pivotRow <- findPivotRowIndex state pivotCol
    return $ updateSimplexState state (pivotCol, pivotRow)

{-
>>> updateSimplexStateTest
Just Basic Column Indices: [4,5,1]
┌                                                                ┐
│    0 % 1    3 % 4    5 % 2    1 % 1    0 % 1 (-1) % 4   21 % 1 │
│    0 % 1    3 % 2    4 % 1    0 % 1    1 % 1 (-1) % 2    6 % 1 │
│    1 % 1    1 % 4    1 % 2    0 % 1    0 % 1    1 % 4    9 % 1 │
│    0 % 1 (-1) % 4 (-1) % 2    0 % 1    0 % 1    3 % 4   27 % 1 │
└                                                                ┘
-}

------------------------------------------------------------------------------------------

-- | Extracts solution vector [x_1, ..., x_n, objective function] from tableau.
getSolutionVector :: SimplexState -> V.Vector Rational
getSolutionVector (SimplexState tab basicColsIdxs) =
  V.fromList $ fmap step [1 .. ncols tab - 1] ++ [V.last lastCol]
  where
    lastCol = getLastCol tab

    step :: Int -> Rational
    step i = maybe 0 (lastCol V.!) (V.elemIndex i basicColsIdxs)

-- | Extracts tableau and solution vector from simplex state,
--   it is expected that optimal solution was found.
getOptimalSolution :: SimplexState -> SimplexResult
getOptimalSolution state = Optimal state $ getSolutionVector state

------------------------------------------------------------------------------------------

-- | Implements the main part of the Simplex algorithm,
--   i.e. finding the pivot and updating the tableau
--   until optimal solution is found.
simplexSolver :: SimplexState -> SimplexStatus
simplexSolver state@(SimplexState tab _) =
  case findPivotColumnIndex tab of
    -- Nothing => Optimal solution was found
    Nothing -> StatusOptimal state
    Just pivotCol -> case findPivotRowIndex state pivotCol of
      -- Nothing => The instance is FeasibleUnbounded
      Nothing -> StatusUnbounded state
      Just pivotRow -> simplexSolver $ updateSimplexState state (pivotCol, pivotRow)

-- | Finds initial feasible solution.
initSimplex :: LPInstance -> ThrowsError SimplexStatus
initSimplex ins@(LPInstance a b c) =
  if all (>= 0) b
    then
      -- Initial feasible solution exists, optimal might not.
      (return . StatusOptimal . initSimplexState) ins
    else
      let state@(SimplexState tab basicColsIdxs) =
            -- vecC = -1 : ..., where -1 because initSimplexState negates vecC
            initSimplexState (ins {matA = fmap (-1 :) a, vecC = -1 : fmap (const 0) c})

          pivotCol = 1
          -- zip with basicColsIndices to follow Bland's rule
          pivotRow = 1 + V.minIndex (V.zip ((V.init . getLastCol) tab) basicColsIdxs)

          -- "all (>= 0) b" holds in this state
          updatedState = updateSimplexState state (pivotCol, pivotRow)
       in case simplexSolver updatedState of
            (StatusUnbounded _) ->
              throwError
                ( NotImplemented
                    "InitSimplex initial instance is FeasibleUnbounded.\
                    \ This case is not implemented yet."
                )
            (StatusInfeasible (SimplexState errorTab _)) ->
              throwError
                ( LogicError
                    ( "SimplexSolver returned Infeasible\
                      \ on initSimplex's initial instance.\
                      \ This should never happen."
                        ++ show errorTab
                    )
                )
            (StatusOptimal s) -> safeTransform s
  where
    safeTransform :: SimplexState -> ThrowsError SimplexStatus
    safeTransform state@(SimplexState tab _) =
      case (V.last . getLastCol) tab of
        0 -> StatusOptimal . updateLastRow <$> removeX0 state
        _ -> return (StatusInfeasible state)

    removeX0 :: SimplexState -> ThrowsError SimplexState
    removeX0 (SimplexState tab basicColsIdxs) =
      -- Throws an error if x_0 is basic,
      -- since this case is not implemented yet.
      case V.elemIndex 1 basicColsIdxs of
        Just _ ->
          throwError
            ( NotImplemented
                ( "InitSimplex found initial feasible solution, but x_0 is basic.\
                  \ This case is not implemented yet.\n"
                    ++ show tab
                    ++ "\n"
                )
            )
        -- SimplexState
        --   { tableau = minorMatrix (rowIdx + 1) 1 tab,
        --     basicColsIndices =
        --       (fmap (subtract 1 . snd) . V.filter ((/= rowIdx) . fst) . V.indexed)
        --         basicColsIdxs
        --   }
        Nothing ->
          return
            ( SimplexState
                { tableau = submatrix 1 (nrows tab) 2 (ncols tab) tab,
                  basicColsIndices = fmap (subtract 1) basicColsIdxs
                }
            )

    -- We add the initial vector C but update it so that all basic columns
    -- have zero as their last component.
    updateLastRow :: SimplexState -> SimplexState
    updateLastRow state@(SimplexState tab basicColsIdxs) =
      state {tableau = foldl step initTableau (V.indexed basicColsIdxs)}
      where
        tabWithoutLastRow = submatrix 1 (nrows tab - 1) 1 (ncols tab) tab

        cWithZeros =
          V.fromList $ fmap ((% 1) . negate) c ++ replicate (ncols tab - length c) 0

        initTableau = tabWithoutLastRow <-> rowVector cWithZeros
        lastRowIdx = nrows initTableau

        step :: Tableau -> (Int, Int) -> Tableau
        step tab' (rowIdx, basicColIdx) =
          let lastComponent = getElem lastRowIdx basicColIdx tab'
           in if lastComponent /= 0
                then combineRows lastRowIdx (-lastComponent) (rowIdx + 1) tab'
                else tab'

{-
>>> initSimplex testInst2
Right (StatusOptimal Basic Column Indices: [3,2]
┌                                              ┐
│    9 % 5    0 % 1    1 % 1 (-1) % 5   14 % 5 │
│ (-1) % 5    1 % 1    0 % 1 (-1) % 5    4 % 5 │
│ (-9) % 5    0 % 1    0 % 1    1 % 5 (-4) % 5 │
└                                              ┘)
-}

-- | Simplex algorithm implementation solving the standard maximum problem.
simplex :: LPInstance -> ThrowsError SimplexResult
simplex ins =
  initSimplex ins
    >>= ( \case
            (StatusUnbounded (SimplexState errorTab _)) ->
              throwError
                ( LogicError
                    ( "InitSimplex returned Unbounded.\
                      \ This should never happen."
                        ++ show errorTab
                    )
                )
            (StatusInfeasible state) -> (return . Infeasible) state
            (StatusOptimal state) -> solve state
        )
  where
    solve :: SimplexState -> ThrowsError SimplexResult
    solve state = case simplexSolver state of
      (StatusUnbounded s) -> (return . FeasibleUnbounded) s
      (StatusInfeasible (SimplexState errorTab _)) ->
        throwError
          ( LogicError
              ( "SimplexSolver returned Infeasible.\
                \ This should never happen."
                  ++ show errorTab
              )
          )
      (StatusOptimal s) -> (return . getOptimalSolution) s

{-
>>> simplex testInst
>>> simplex testInst2
Right Solution Vector: [8 % 1,4 % 1,0 % 1,18 % 1,0 % 1,0 % 1,28 % 1]
┌                                                                ┐
│    0 % 1    0 % 1    1 % 2    1 % 1 (-1) % 2    0 % 1   18 % 1 │
│    0 % 1    1 % 1    8 % 3    0 % 1    2 % 3 (-1) % 3    4 % 1 │
│    1 % 1    0 % 1 (-1) % 6    0 % 1 (-1) % 6    1 % 3    8 % 1 │
│    0 % 1    0 % 1    1 % 6    0 % 1    1 % 6    2 % 3   28 % 1 │
└                                                                ┘
Right Solution Vector: [14 % 9,10 % 9,0 % 1,0 % 1,2 % 1]
┌                                              ┐
│    1 % 1    0 % 1    5 % 9 (-1) % 9   14 % 9 │
│    0 % 1    1 % 1    1 % 9 (-2) % 9   10 % 9 │
│    0 % 1    0 % 1    1 % 1    0 % 1    2 % 1 │
└                                              ┘
-}
