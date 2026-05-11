module Main where

import LPSolver.Parser (readLPInstance)
import LPSolver.Simplex (simplex)
import LPSolver.Types (safeExtractValue)
import System.Environment (getArgs)

------------------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  if null args
    then
      putStrLn
        "Error: Enter file path or matrix A, vector B and vector C as three string arguments."
    else
      solveInstance $ args !! 0
  where
    solveInstance :: String -> IO ()
    solveInstance =
      putStrLn . safeExtractValue . fmap (show . simplex) . readLPInstance