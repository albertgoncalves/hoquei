{-# OPTIONS_GHC -Wall #-}

import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Text.Printf (printf)
import Text.Read (readMaybe)

(|.) :: (a -> b) -> (b -> c) -> (a -> c)
f |. g = g . f

(|>) :: a -> (a -> b) -> b
x |> f = f x

split :: Char -> String -> [String]
split d x = f (reverse x) [] []
  where
    f [] y ys = y : ys
    f (x' : xs) y ys
        | (x' == d) || isSpace x' =
            case y of
                "" -> f xs [] ys
                y' -> f xs [] (y' : ys)
        | otherwise = f xs (x' : y) ys

filterTail :: (a -> Bool) -> [a] -> [a]
filterTail _ [] = []
filterTail f (x : xs) = x : filter f xs

format :: Int -> Float -> String
format 0 = show . (round :: Float -> Int)
format i = printf "%d:%f" i

pipeline :: String -> String
pipeline =
    split ','
    |. map (readMaybe :: String -> Maybe Float)
    |. zip [(0 :: Int) ..]
    |. map sequence
    |. catMaybes
    |. filterTail (\(_, x) -> x /= 0)
    |. map (uncurry format)
    |. unwords

main :: IO ()
main = interact pipeline
