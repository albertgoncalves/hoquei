{-# OPTIONS_GHC -Wall #-}

import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Data.Text (pack, split, unpack)
import Text.Printf (printf)
import Text.Read (readMaybe)

(|.) :: (a -> b) -> (b -> c) -> (a -> c)
f |. g = g . f

delimiter :: Char -> Bool
delimiter x = (x == ',') || isSpace x

index :: [a] -> [(Int, a)]
index = zip [0 ..]

filterTail :: (a -> Bool) -> [a] -> [a]
filterTail _ [] = []
filterTail f (x : xs) = x : filter f xs

sparse :: (a, Float) -> Bool
sparse (_, x) = x /= 0

format :: Int -> Float -> String
format 0 = show . (round :: Float -> Int)
format i = printf "%d:%f" i

pipeline :: String -> String
pipeline =
    pack
    |. split delimiter
    |. map unpack
    |. filter (/= "")
    |. map (readMaybe :: String -> Maybe Float)
    |. index
    |. map sequence
    |. catMaybes
    |. filterTail sparse
    |. map (uncurry format)
    |. unwords

main :: IO ()
main = interact pipeline
