module Main where

import LPSolver.Parser (readLPInstances)
import LPSolver.Types (safeExtractValue)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hIsTerminalDevice, hPutStrLn, stderr, stdin)

------------------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs

  if any (`elem` ["-h", "--help"]) args
    then do
      printHelp
      exitSuccess
    else do
      isTerminal <- hIsTerminalDevice stdin
      case (isTerminal, null args) of
        (True, True) -> do
          hPutStrLn stderr "Error: No input detected."
          exitFailure
        (False, _) -> do
          contents <- getContents
          solveInstances $ unlines args ++ contents
        _ -> solveInstances $ unlines args
  where
    solveInstances :: String -> IO ()
    solveInstances = putStrLn . safeExtractValue . fmap show . readLPInstances

    --  Prints the help message
    printHelp :: IO ()
    printHelp = do
      putStrLn $ "Usage: " ++ "cabal run lp-solver --" ++ " [OPTIONS] [ARGUMENTS]"
      putStrLn "Linear programming standard maximum problem solver implementing the Simplex algorithm."
      putStrLn ""
      putStrLn "Options:"
      putStrLn "  -h, --help    Show this help message and exit"
      putStrLn ""
      putStrLn "Examples:"
      putStrLn
        "  cabal run lp-solver --\
        \ \"{[[a_11, ..., a_1n], ..., [a_m1, ..., a_mn]], \
        \ [b_1, ..., b_m], [c_1, ..., c_n]}\" ... \"{...}\""
      putStrLn
        "  cat input.txt |\
        \ cabal run lp-solver"
      putStrLn
        "  cat input.txt |\
        \ cabal run lp-solver\
        \ > output.txt"
      putStrLn
        "  cat input.txt |\
        \ cabal run lp-solver --\
        \ \"{[[a_11, ..., a_1n], ..., [a_m1, ..., a_mn]],\
        \ [b_1, ..., b_m], [c_1, ..., c_n]}\" ... \"{...}\""
