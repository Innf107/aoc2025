{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

import Data.Function ((&))

main :: IO ()
main = do
    input <- readFile "input.txt"
    let movements =
            lines input & map \case
                'L' : rest -> -(read rest)
                'R' : rest -> read rest
                _ -> 0
    let applyMovement (total, count) movement = do
            let next = (total + movement) `mod` 100
            if next == 0
                then (next, count + 1)
                else (next, count)
    print (foldl' applyMovement (50, 0) movements)
