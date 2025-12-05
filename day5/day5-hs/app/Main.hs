{-# LANGUAGE GHC2021, OverloadedRecordDot, OverloadedStrings, LambdaCase #-}
module Main (main) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.List (List)
import Data.List qualified as List

parseInputLines :: List Text -> (List (Int, Int), List Int)
parseInputLines = go []
    where
        go intervals = \case 
            [] -> error "invalid input"
            ("" : rest) -> (List.sort intervals, map (read @Int . Text.unpack) rest)
            (interval : rest) -> case Text.splitOn "-" interval of
                [lower, upper] -> go ((read (Text.unpack lower), read (Text.unpack upper)) : intervals) rest
                _ -> error "invalid input"

buildIntervalMap :: List (Int, Int) -> IntMap Int
buildIntervalMap = foldl' insertSorted mempty
    where
        insertSorted map (lower, upper) = case IntMap.lookupLE lower map of
            Nothing -> IntMap.insert lower upper map
            Just (previousLower, previousUpper)
                | previousUpper >= upper -> map
                | previousUpper < lower -> IntMap.insert lower upper map
                | otherwise -> IntMap.insert previousLower upper map
            
isContainedIn :: Int -> IntMap Int -> Bool
isContainedIn point intervalMap =
    case IntMap.lookupLE point intervalMap of
        Nothing -> False
        Just (_lower, upper) -> point <= upper

part1 :: IO ()
part1 = do
    input <- Text.readFile "input.txt"
    let (intervals, points) = parseInputLines (Text.lines input)

    let intervalMap = buildIntervalMap intervals

    print $ length $ filter (`isContainedIn` intervalMap) points

part2 :: IO ()
part2 = do
    input <- Text.readFile "input.txt"
    let (intervals, _points) = parseInputLines (Text.lines input)

    let intervalMap = buildIntervalMap intervals

    print $ sum $ map (\(lower, upper) -> upper - lower + 1) (IntMap.toList intervalMap)

main :: IO ()
main = part1
