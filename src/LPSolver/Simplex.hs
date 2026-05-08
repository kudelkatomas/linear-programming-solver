module LPSolver.Simplex where

import Data.Matrix
  ( Matrix (ncols),
    combineRows,
    fromList,
    fromLists,
    getCol,
    getElem,
    identity,
    nrows,
    scaleRow,
    (<->),
    (<|>),
  )
import Data.Ratio ((%))
import qualified Data.Vector as V
  ( Vector,
    elemIndex,
    findIndex,
    fromList,
    init,
    last,
    maxIndex,
    zip,
    zipWith,
    (!),
    (//),
  )
import LPSolver.Types
  ( LPInstance (..),
    SimplexResult (..),
    SimplexState (..),
    Tableau,
    getLastCol,
    getLastRow,
  )

------------------------------------------------------------------------------------------

-- For testing purposes
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

------------------------------------------------------------------------------------------

-- | Combines matA, vecB, vecC and an identity matrix into a tableau and
--   initializes basic columns indices
initSimplexState :: LPInstance -> SimplexState
initSimplexState (LPInstance a b c) =
  SimplexState
    { tableau = (% 1) <$> aIdentityC <|> bWithZero,
      basicColsIndices = basicColsIndices'
    }
  where
    aMat = fromLists a
    aNRows = nrows aMat
    cWithZeros = fmap (* (-1)) c ++ replicate aNRows 0
    bWithZero = fromList (length b + 1) 1 (b ++ [0])
    aIdentityC =
      (aMat <|> identity aNRows) <-> fromList 1 (length c + aNRows) cWithZeros
    basicColsIndices' = V.fromList [aNRows + i | i <- [1 .. aNRows]]

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
--   Nothing if every c_j \<= 0 (i.e. -c_j \>=0)
findPivotColumnIndex :: Tableau -> Maybe Int
findPivotColumnIndex = fmap (+ 1) . V.findIndex (< 0) . V.init . getLastRow

-- | Finds pivot's row index following the Bland's rule or
--   Nothing if a_{column, row_i} \<= 0 for every i
findPivotRowIndex :: SimplexState -> Int -> Maybe Int
findPivotRowIndex (SimplexState tab basicColsIdxs) pivotColIdx =
  fst (ratios V.! minIdx) >> Just (minIdx + 1)
  where
    -- Drop the last row
    b = V.init $ getLastCol tab
    col = V.init $ getCol pivotColIdx tab

    -- Nothing < Just _, therefore ratios multiplied by -1 and maxIndex.
    -- zip with basicColsIndices (* -1 ... maxIndex) to follow Bland's rule.
    ratios = V.zip (V.zipWith safeNegRatio b col) (fmap (* (-1)) basicColsIdxs)
    minIdx = V.maxIndex ratios

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

-- | Given the pivot's coordinates (column, row) performs one tableau update
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

-- | Extracts solution vector [x_1, ..., x_n, objective function] from tableau
getSolutionVector :: SimplexState -> V.Vector Rational
getSolutionVector (SimplexState tab basicColsIdxs) =
  V.fromList $ fmap step [1 .. ncols tab - 1] ++ [V.last lastCol]
  where
    lastCol = getLastCol tab

    step :: Int -> Rational
    step i = maybe 0 (lastCol V.!) (V.elemIndex i basicColsIdxs)

------------------------------------------------------------------------------------------

simplex :: LPInstance -> SimplexResult
simplex ins = maybe Infeasible solver initSimplex
  where
    -- DOPSAT DOKUMENTACI
    initSimplex :: Maybe SimplexState
    initSimplex = Just $ initSimplexState ins

    solver :: SimplexState -> SimplexResult
    solver state@(SimplexState tab _) =
      case findPivotColumnIndex tab of
        Nothing -> Optimal tab $ getSolutionVector state
        Just pivotCol ->
          case findPivotRowIndex state pivotCol of
            Nothing -> FeasibleUnbounded
            Just pivotRow ->
              solver $ updateSimplexState state (pivotCol, pivotRow)

{-
>>> simplex testInst
Solution Vector: [8 % 1,4 % 1,0 % 1,18 % 1,0 % 1,0 % 1,28 % 1]
┌                                                                ┐
│    0 % 1    0 % 1    1 % 2    1 % 1 (-1) % 2    0 % 1   18 % 1 │
│    0 % 1    1 % 1    8 % 3    0 % 1    2 % 3 (-1) % 3    4 % 1 │
│    1 % 1    0 % 1 (-1) % 6    0 % 1 (-1) % 6    1 % 3    8 % 1 │
│    0 % 1    0 % 1    1 % 6    0 % 1    1 % 6    2 % 3   28 % 1 │
└                                                                ┘
-}
