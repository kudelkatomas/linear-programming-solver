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
import Data.Vector (Vector, maxIndex, minIndex, toList, zipWith, (!))
import LPSolver.Types (LPInstance (..), Tableau, getLastCol, getLastRow)

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
      vecC = [-3, -1, -2]
    }

------------------------------------------------------------------------------------------

-- | Combines matA, vecB, vecC and an identity matrix into a tableau
buildTableau :: LPInstance -> Tableau
buildTableau (LPInstance a b c) = (% 1) <$> aIdentityC <|> bWithZero
  where
    aMat = fromLists a
    aNRows = nrows aMat
    cWithZeros = c ++ replicate aNRows 0
    bWithZero = fromList (length b + 1) 1 (b ++ [0])
    aIdentityC =
      (aMat <|> identity aNRows) <-> fromList 1 (length c + aNRows) cWithZeros

{-
>>> buildTableau testInst
┌                                                                ┐
│    1 % 1    1 % 1    3 % 1    1 % 1    0 % 1    0 % 1   30 % 1 │
│    2 % 1    2 % 1    5 % 1    0 % 1    1 % 1    0 % 1   24 % 1 │
│    4 % 1    1 % 1    2 % 1    0 % 1    0 % 1    1 % 1   36 % 1 │
│ (-3) % 1 (-1) % 1 (-2) % 1    0 % 1    0 % 1    0 % 1    0 % 1 │
└                                                                ┘
-}

------------------------------------------------------------------------------------------

findPivotColumnIndex :: Tableau -> Maybe Int
findPivotColumnIndex tbl = if lastRow ! minIdx < 0 then Just (minIdx + 1) else Nothing
  where
    lastRow = getLastRow tbl
    minIdx = minIndex lastRow

findPivotRowIndex :: Tableau -> Int -> Maybe Int
findPivotRowIndex tbl pivotColIdx = ratios ! minIdx >> Just (minIdx + 1)
  where
    -- Nothing < Just _, therefore ratios multiplied by -1 and maxIndex
    ratiosDiv :: Rational -> Rational -> Maybe Rational
    ratiosDiv n d = if d > 0 then Just (n / (-d)) else Nothing

    ratios = Data.Vector.zipWith ratiosDiv (getLastCol tbl) (getCol pivotColIdx tbl)
    minIdx = maxIndex ratios

-- | Finds pivot coordinates (column, row) indexed from 1
findPivotPos :: Tableau -> Maybe (Int, Int)
findPivotPos tbl =
  do
    col <- findPivotColumnIndex tbl
    row <- findPivotRowIndex tbl col
    return (col, row)

{-
>>> findPivotPos $ buildTableau testInst
Just (1,3)
-}

------------------------------------------------------------------------------------------

-- | Given the pivot's coordinates (column, row) performs one tableau update
updateTableau :: Tableau -> (Int, Int) -> Tableau
updateTableau tbl (col, row) = foldl updateRow scaledTbl targetRows
  where
    targetRows = [i | i <- [1 .. nrows tbl], i /= row]
    scaledTbl =
      let pivot = getElem row col tbl
       in scaleRow (1 / pivot) row tbl

    updateRow :: Tableau -> Int -> Tableau
    updateRow tbl' rowIdx = combineRows rowIdx (-(getElem rowIdx col tbl')) row tbl'

updateTableauTest :: Maybe Tableau
updateTableauTest =
  do
    let tbl = buildTableau testInst
    pos <- findPivotPos tbl
    return $ updateTableau tbl pos

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

simplex :: LPInstance -> Tableau
simplex ins = helper $ buildTableau ins
  where
    helper :: Tableau -> Tableau
    helper tbl =
      case findPivotPos tbl of
        Nothing -> tbl
        Just pivotPos -> helper $ updateTableau tbl pivotPos

{-
>>> simplex testInst
┌                                                                ┐
│    0 % 1    0 % 1    1 % 2    1 % 1 (-1) % 2    0 % 1   18 % 1 │
│    0 % 1    1 % 1    8 % 3    0 % 1    2 % 3 (-1) % 3    4 % 1 │
│    1 % 1    0 % 1 (-1) % 6    0 % 1 (-1) % 6    1 % 3    8 % 1 │
│    0 % 1    0 % 1    1 % 6    0 % 1    1 % 6    2 % 3   28 % 1 │
└                                                                ┘
-}

------------------------------------------------------------------------------------------

-- | Extracts solution vector [x_1, ..., x_n, objective function] from tableau
solutionVector :: Tableau -> [Rational]
solutionVector tbl = [if i /= -1 then lastCol ! i else 0 | i <- basicRowIndices]
  where
    lastCol = getLastCol tbl

    basicRowIndices =
      [findBasicRowIndex $ getCol i tbl | i <- [1 .. ncols tbl]]
        ++ [nrows tbl - 1] -- for the value of the objective function

    -- -1 if given vector is not basic, otherwise the index of the row with the one
    findBasicRowIndex :: Vector Rational -> Int
    findBasicRowIndex col = foldl step (-2) (zip [(0 :: Int) ..] $ toList col)

    step :: Int -> (Int, Rational) -> Int
    step (-1) _ = -1
    step (-2) (i, 1) = i
    step j (_, 0) = j
    step _ _ = -1

{-
>>> solutionVector $ simplex testInst
[8 % 1,4 % 1,0 % 1,18 % 1,0 % 1,0 % 1,0 % 1,28 % 1]
-}
