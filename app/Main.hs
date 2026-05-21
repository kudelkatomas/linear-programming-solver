module Main where

import Data.List (partition)
import LPSolver.Parser (readLPInstances)
import LPSolver.Simplex (simplex)
import LPSolver.Types
  ( LPInstance,
    SimplexResult,
    ThrowsError,
    safeExtractString,
    safeShowCompactValue,
    safeShowValue,
  )
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hIsTerminalDevice, hPutStrLn, stderr, stdin)

------------------------------------------------------------------------------------------

main :: IO ()
main = do
  rawArgs <- getArgs

  if any (`elem` ["-h", "--help"]) rawArgs
    then do
      printHelp
      exitSuccess
    else do
      let (verboseOptions, args) = partition (`elem` ["-v", "--verbose"]) rawArgs
          isVerbose = not (null verboseOptions)

      isTerminal <- hIsTerminalDevice stdin
      case (isTerminal, null args) of
        (True, True) -> do
          hPutStrLn stderr "Error: No input detected."
          exitFailure
        (False, _) -> do
          contents <- getContents
          (writeOutput isVerbose . solveInstances) $ unlines args ++ contents
        _ -> (writeOutput isVerbose . solveInstances) $ unlines args
  where
    solveInstances :: String -> ThrowsError [(LPInstance, ThrowsError SimplexResult)]
    solveInstances input = do
      instances <- readLPInstances input
      let results = fmap simplex instances
      return $ zip instances results

    writeOutput :: Bool -> ThrowsError [(LPInstance, ThrowsError SimplexResult)] -> IO ()
    writeOutput isVerbose =
      putStr
        . safeExtractString
        . fmap (concatMap (formatSolution isVerbose))

    formatSolution :: Bool -> (LPInstance, ThrowsError SimplexResult) -> String
    formatSolution isVerbose (ins, res) =
      let showRes = if isVerbose then safeShowValue else safeShowCompactValue
       in "{\n" ++ show ins ++ "\n" ++ showRes res ++ "\n}\n"

    --  Prints the help message
    printHelp :: IO ()
    printHelp =
      putStr $
        unlines
          [ "Usage: cabal run lp-solver -- [OPTIONS] [ARGUMENTS]",
            "Linear programming standard maximum problem solver implementing the Simplex algorithm.",
            "",
            "Options:",
            "  -h, --help       Show this help message and exit",
            "  -v, --verbose    Prints the final tableau for every result",
            "",
            "Examples:",
            "  cabal run lp-solver -- \"{[[a_11, ..., a_1n], ..., [a_m1, ..., a_mn]], [b_1, ..., b_m], [c_1, ..., c_n]}\"",
            "  cat input.txt | cabal run lp-solver",
            "  cat input.txt | cabal run lp-solver > output.txt",
            "  cat input.txt | cabal run lp-solver -- -v \"{[[a_11, ..., a_1n], ...]}\" > output.txt"
          ]