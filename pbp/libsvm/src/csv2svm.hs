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
sparse = (/= 0) . snd

format :: Int -> Float -> String
format 0 = show . (truncate :: Float -> Int)
format i = printf "%d:%f" i

convert :: String -> String -> String
convert delimiter =
    pack
    |. splitOn (pack delimiter)
    |. map ((readMaybe :: String -> Maybe Float) . unpack)
    |. zip [(0 :: Int) ..]
    |. map sequence
    |. catMaybes
    |. filterTail sparse
    |. map (uncurry format)
    |. unwords

mapLines :: (String -> String) -> String -> String
mapLines f = unlines . filter (/= "") . map f . lines

printUsage :: IO ()
printUsage = putStr message >> exitWith (ExitFailure 1)
  where
    message =
        unlines
            [ "usage: convert [-d DELIMITER]"
            , "input: stdin"
            , "output: stdout"
            ]

processStdin :: String -> IO ()
processStdin delimiter =
    getContents >>= putStr . mapLines (convert delimiter) >> exitSuccess

parseArgs :: [String] -> IO ()
parseArgs ["-d"] = printUsage
parseArgs ["-d", delimiter] = processStdin delimiter
parseArgs [] = processStdin ","
parseArgs _ = printUsage

main :: IO ()
main = getArgs >>= parseArgs
