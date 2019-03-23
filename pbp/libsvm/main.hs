{-# OPTIONS_GHC -Wall #-}

import Data.Char (isSpace)
import Text.Printf (printf)

(|.) :: (a -> b) -> (b -> c) -> (a -> c)
f |. g = g . f

(|>) :: a -> (a -> b) -> b
x |> f = f x

split :: Char -> String -> [String]
split d x = f (reverse x) [] []
  where
    f [] y ys = y : ys
    f (x':xs) y ys
        | (x' == d) || isSpace x' =
            case y of
                "" -> f xs [] ys
                y' -> f xs [] (y' : ys)
        | otherwise = f xs (x' : y) ys

sparse :: Int -> Float -> String
sparse 0 x = x |> round' |> show
  where
    round' :: Float -> Int
    round' = round
sparse i x = printf "%d:%f" i x

filterTail :: (a -> Bool) -> [a] -> [a]
filterTail f xs = h : filter f t
  where
    h = head xs
    t = tail xs

pipeline :: String -> String
pipeline =
    split ','
    |. map read
    |. zip [(0 :: Int) ..]
    |. filterTail (\(_, x) -> x /= 0)
    |. map (uncurry sparse)
    |. unwords

beforeAfter :: String -> IO ()
beforeAfter x = mapM_ print [x, pipeline x]

main :: IO ()
main = beforeAfter "0 1.01, 0.0001,\n2,0.0,0.0000000000000, 10.99"
