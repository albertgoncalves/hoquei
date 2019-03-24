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

convert :: String -> String -> String
convert delimiter =
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

mapLines :: (String -> String) -> String -> String
mapLines f =
    lines
    |. map f
    |. filter (/= "")
    |. unlines

printUsage :: IO a
printUsage = do
    putStr message
    exitWith (ExitFailure 1)
  where
    message =
        unlines
            [ "usage: convert [-d DELIMITER]"
            , "input: stdin"
            , "output: stdout"
            ]

processStdin :: String -> IO a
processStdin delimiter = do
    getContents >>= putStr . mapLines (convert delimiter)
    exitSuccess

parseArgs :: [String] -> IO a
parseArgs ["-d"] = printUsage
parseArgs ["-d", delimiter] = processStdin delimiter
parseArgs [] = processStdin ","
parseArgs _ = printUsage

main :: IO ()
main = getArgs >>= parseArgs
