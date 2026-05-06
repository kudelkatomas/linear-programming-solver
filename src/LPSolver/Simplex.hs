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
    findIndex,
    fromList,
    init,
    maxIndex,
    toList,
    zipWith,
    (!),
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

-- | Combines matA, vecB, vecC and an identity matrix into a tableau
buildTableau :: LPInstance -> SimplexState
buildTableau (LPInstance a b c) =
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
>>> buildTableau testInst
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
findPivotColumnIndex = helper . getLastRow
  where
    helper :: V.Vector Rational -> Maybe Int
    helper lastRow = (+ 1) <$> V.findIndex (< 0) (V.init lastRow)

-- | Finds pivot's row index following the Bland's rule or
--   Nothing if a_{column, row_i} \<= 0 for every i
findPivotRowIndex :: SimplexState -> Int -> Maybe Int
findPivotRowIndex (SimplexState tab basicColsIndices) pivotColIdx =
  ratios V.! minIdx >> Just (minIdx + 1)
  where
    -- Drop the last row
    b = V.init $ getLastCol tab
    col = V.init $ getCol pivotColIdx tab

    -- Nothing < Just _, therefore ratios multiplied by -1 and maxIndex
    ratios = V.zipWith safeNegRatio b col

    -- Bland's rule is NOT followed
    minIdx = V.maxIndex ratios

    safeNegRatio :: Rational -> Rational -> Maybe Rational
    safeNegRatio n d = if d > 0 then Just (n / (-d)) else Nothing

    ratiosMax = undefined

------------------------------------------------------------------------------------------

-- | Given the pivot's coordinates (column, row) performs one tableau update
updateTableau :: SimplexState -> (Int, Int) -> SimplexState
updateTableau tab (col, row) = foldl updateRow scaledTab targetRows
  where
    targetRows = [i | i <- [1 .. nrows tab], i /= row]
    scaledTab =
      let pivot = getElem row col tab
       in scaleRow (1 / pivot) row tab

    updateRow :: Tableau -> Int -> Tableau
    updateRow tab' rowIdx = combineRows rowIdx (-(getElem rowIdx col tab')) row tab'

updateTableauTest :: Maybe Tableau
updateTableauTest =
  do
    let tab = buildTableau testInst
    pos <- findPivotPos tab
    return $ updateTableau tab pos

{-
>>> updateTableauTest
Just ┌                                                                ┐
│    0 % 1    3 % 4    5 % 2    1 % 1    0 % 1 (-1) % 4   21 % 1 │
│    0 % 1    3 % 2    4 % 1    0 % 1    1 % 1 (-1) % 2    6 % 1 │
│    1 % 1    1 % 4    1 % 2    0 % 1    0 % 1    1 % 4    9 % 1 │
│    0 % 1 (-1) % 4 (-1) % 2    0 % 1    0 % 1    3 % 4   27 % 1 │
└                                                                ┘
-}

------------------------------------------------------------------------------------------

-- | Extracts solution vector [x_1, ..., x_n, objective function] from tableau
getSolutionVector :: Tableau -> V.Vector Rational
getSolutionVector tab = [if i /= -1 then lastCol V.! i else 0 | i <- basicRowIndices]
  where
    lastCol = getLastCol tab

    basicRowIndices =
      [findBasicRowIndex $ getCol i tab | i <- [1 .. ncols tab]]
        ++ [nrows tab - 1] -- for the value of the objective function

    -- -1 if given vector is not basic, otherwise the index of the row with the one
    findBasicRowIndex :: V.Vector Rational -> Int
    findBasicRowIndex col = foldl step (-2) (zip [(0 :: Int) ..] $ V.toList col)

    step :: Int -> (Int, Rational) -> Int
    step (-1) _ = -1
    step (-2) (i, 1) = i
    step j (_, 0) = j
    step _ _ = -1

{-
>>> getSolutionVector $ simplex testInst
[8 % 1,4 % 1,0 % 1,18 % 1,0 % 1,0 % 1,0 % 1,28 % 1]
-}

------------------------------------------------------------------------------------------

simplex :: LPInstance -> SimplexResult
simplex ins = maybe Infeasible solver initSimplex
  where
    -- DOPSAT DOKUMENTACI
    initSimplex :: Maybe SimplexState
    initSimplex = Just $ buildTableau ins

    solver :: SimplexState -> SimplexResult
    solver state@(SimplexState tab _) =
      case findPivotColumnIndex tab of
        Nothing -> Optimal tab $ getSolutionVector tab
        Just pivotCol ->
          case findPivotRowIndex state pivotCol of
            Nothing -> FeasibleUnbounded
            Just pivotRow ->
              solver $ updateTableau state (pivotCol, pivotRow)

{-
>>> simplex testInst
┌                                                                ┐
│    0 % 1    0 % 1    1 % 2    1 % 1 (-1) % 2    0 % 1   18 % 1 │
│    0 % 1    1 % 1    8 % 3    0 % 1    2 % 3 (-1) % 3    4 % 1 │
│    1 % 1    0 % 1 (-1) % 6    0 % 1 (-1) % 6    1 % 3    8 % 1 │
│    0 % 1    0 % 1    1 % 6    0 % 1    1 % 6    2 % 3   28 % 1 │
└                                                                ┘
-}
