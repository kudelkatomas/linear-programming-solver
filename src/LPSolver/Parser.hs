module LPSolver.Parser (readLPInstances) where

import Control.Monad.Except (MonadError (throwError))
import LPSolver.Types (LPInstance (..), SimplexError (..), ThrowsError)
import Text.ParserCombinators.Parsec
  ( Parser,
    char,
    digit,
    eof,
    many,
    many1,
    option,
    parse,
    sepBy,
    skipMany,
    space,
    string,
    try,
  )

------------------------------------------------------------------------------------------

-- | Consumes zero or more spaces.
optionalSpaces :: Parser ()
optionalSpaces = skipMany space

-- | Recognizes an integer.
parseInteger :: Parser Integer
parseInteger = do
  sign <- option "" (string "-")
  digits <- many1 digit
  return $ read (sign ++ digits)

------------------------------------------------------------------------------------------

-- | Recognizes a vector in [v_1, ..., v_n] format.
parseVector :: Parser [Integer]
parseVector = do
  _ <- char '['
  optionalSpaces

  vals <- sepBy parseInteger (try (optionalSpaces >> char ',' >> optionalSpaces))

  optionalSpaces
  _ <- char ']'

  return vals

-- | Recognizes a matrix in [[a_11, ..., a_1n], ..., [a_m1, ..., a_mn]] format,
--   i.e. list of rows.
parseMatrix :: Parser [[Integer]]
parseMatrix = do
  _ <- char '['
  optionalSpaces

  rows <- sepBy parseVector (try (optionalSpaces >> char ',' >> optionalSpaces))

  optionalSpaces
  _ <- char ']'

  return rows

------------------------------------------------------------------------------------------

-- | Recognizes one LPInstance.
parseLPInstance :: Parser LPInstance
parseLPInstance = do
  _ <- char '{'
  optionalSpaces

  a <- parseMatrix
  optionalSpaces
  _ <- char ','
  optionalSpaces

  b <- parseVector
  optionalSpaces
  _ <- char ','
  optionalSpaces

  c <- parseVector
  optionalSpaces
  _ <- char '}'

  return LPInstance {matA = a, vecB = b, vecC = c}

------------------------------------------------------------------------------------------

-- | Recognizes multiple LPInstances.
parseLPInstances :: Parser [LPInstance]
parseLPInstances = do
  optionalSpaces

  instances <-
    many
      ( do
          inst <- parseLPInstance
          optionalSpaces
          return inst
      )

  eof
  return instances

------------------------------------------------------------------------------------------

-- | Tries to parse 'LPInstance's from a string.
readLPInstances :: String -> ThrowsError [LPInstance]
readLPInstances input = case parse parseLPInstances "simplex" input of
  Left err -> throwError $ Parser err
  Right val -> return val

------------------------------------------------------------------------------------------