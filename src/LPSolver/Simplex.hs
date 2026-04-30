module LPSolver.Simplex where

import Data.Matrix (Matrix, fromLists)

mat :: Matrix Integer
mat =
  fromLists
    [ [1, 1, 3, 30],
      [2, 2, 5, 24],
      [4, 1, 2, 36],
      [-3, -1, -2, 0]
    ]