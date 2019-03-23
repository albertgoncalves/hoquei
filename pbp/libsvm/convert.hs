{-# OPTIONS_GHC -Wall #-}

import Data.Char (isAlphaNum)
import Data.Maybe (catMaybes)
import Data.Text (pack, split, unpack)
import Text.Printf (printf)
import Text.Read (readMaybe)

(|.) :: (a -> b) -> (b -> c) -> (a -> c)
f |. g = g . f

delimiter :: Char -> Bool
delimiter x = not $ isAlphaNum x || (x == '.')

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
    filter (/= ' ')
    |. pack
    |. split delimiter
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

main :: IO ()
main = interact $ process pipeline
