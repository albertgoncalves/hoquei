{-# OPTIONS_GHC -Wall #-}

import Data.Maybe (catMaybes)
import Data.Text (pack, splitOn, unpack)
import Text.Printf (printf)
import Text.Read (readMaybe)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitSuccess, exitWith)

(|.) :: (a -> b) -> (b -> c) -> (a -> c)
f |. g = g . f

filterTail :: (a -> Bool) -> [a] -> [a]
filterTail _ [] = []
filterTail f (x : xs) = x : filter f xs

sparse :: (a, Float) -> Bool
sparse (_, x) = x /= 0

format :: Int -> Float -> String
format 0 = show . (round :: Float -> Int)
format i = printf "%d:%f" i

pipeline :: String -> String -> String
pipeline delimiter =
    pack
    |. splitOn (pack delimiter)
    |. map unpack
    |. map (readMaybe :: String -> Maybe Float)
    |. zip [(0 :: Int) ..]
    |. map sequence
    |. catMaybes
    |. filterTail sparse
    |. map (uncurry format)
    |. unwords

process :: (String -> String) -> String -> String
process f =
    lines
    |. map f
    |. filter (/= "")
    |. unlines

halt :: IO a
halt = do
    putStr message
    exitWith (ExitFailure 1)
  where
    message =
        unlines
            [ "usage: convert [-d DELIMITER]"
            , "input: stdin"
            , "output: stdout"
            ]

run :: String -> IO a
run delimiter = do
    getContents >>= putStr . process (pipeline delimiter)
    exitSuccess

parse :: [String] -> IO a
parse ["-d"] = halt
parse ["-d", delimiter] = run delimiter
parse [] = run ","
parse _ = halt

main :: IO ()
main = getArgs >>= parse
