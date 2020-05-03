-- Google Code Jam Round 1c - Overexcited Fan
-- https://codingcompetitions.withgoogle.com/codejam/round/000000000019fef4/0000000000317409
-- vim: foldmethod=marker

-- boilerplate {{{1

module Main where

import Control.Monad ( forM_  )
import Text.Printf   ( printf )

main :: IO ()
main = do
    t <- read <$> getLine :: IO Int
    forM_ [1..t] $ \i -> printf "Case #%d: " i >> docase

-- solution {{{1

docase :: IO ()
docase = do
    [xs,ys,path] <- words <$> getLine :: IO [String]
    let x = read xs; y = read ys
    maybe (putStrLn "IMPOSSIBLE") print $ solve x y path

solve :: Int -> Int -> String -> Maybe Int
solve = go 0 where
    go t x y _ | abs x + abs y <= t = Just t
    go _ _ _ []                     = Nothing
    go t x y (d:path) = go (t+1) x' y' path where
        (x',y') = case d of
            'N' -> (x,y+1)
            'S' -> (x,y-1)
            'E' -> (x+1,y)
            'W' -> (x-1,y)
            c -> error $ printf "invalid path direction: %s" (show c)
