module Latency
  ( latency_for_instruction,
    latency_lookup,
    LatencyOperand(..),
    LatencyRecord(..) ) where

import Data.List.Split
import Data.Char
import Instr

data LatencyOperand = LReg (Maybe Int) | LMem (Maybe Int) | LImm | LCl | LUnknown | LAx 
                        | LXmm | Lmm | LYmm deriving (Show)
data LatencyRecord = Mk_lr String [LatencyOperand] Float deriving (Show)

processRow::[String] -> [LatencyRecord]
processRow list = let opcodes = words $ map toLower (list !! 0)
                      operands = parseOperands (list !! 1)
                      latency1 = if length list > 9 then parseNumber (list !! 9) else Nothing
                      latency2 = if length list > 10 then parseNumber (list !! 10) else Nothing
                      latency = average latency1 latency2 in
                  case latency of
                    Nothing -> []
                    Just l  -> [ Mk_lr oc ors l | oc <- opcodes, ors <- operands]

average :: Fractional a => Maybe a -> Maybe a -> Maybe a
average x y = case (x,y) of
                (Nothing, Nothing) -> Nothing
                (Just a, Just b) -> Just ((a+b)/2)
                (Just a, Nothing) -> Just a
                (Nothing, Just b) -> Just b

parseNumber::String -> Maybe Float
parseNumber str = foldl1 (average) $ map parseNumber' (splitOn "-" str)

parseNumber' :: String -> Maybe Float
parseNumber' ('~':cs) = parseNumber' cs
parseNumber' cs = readMaybe cs :: Maybe Float

parseOperands::String -> [[LatencyOperand]]
parseOperands ls = restructure $ map parseOperand (splitOn "," ls)

parseOperand :: String -> [LatencyOperand]
parseOperand s = concat $ map parseOperand' $ splitOn "/" s

parseOperand' :: String -> [LatencyOperand]
parseOperand' "r" = [LReg Nothing]
parseOperand' "r8" = [LReg (Just 8)]
parseOperand' "r16" = [LReg (Just 16)]
parseOperand' "r32" = [LReg (Just 32)]
parseOperand' "r64" = [LReg (Just 64)]
parseOperand' "r80" = [LReg (Just 80)]

parseOperand' "m" = [LMem Nothing]
parseOperand' "m8" = [LMem (Just 8)]
parseOperand' "m16" = [LMem (Just 16)]
parseOperand' "m32" = [LMem (Just 32)]
parseOperand' "m64" = [LMem (Just 64)]
parseOperand' "m80" = [LMem (Just 80)]
parseOperand' "m128" = [LMem (Just 64)]
parseOperand' "m256" = [LMem (Just 64)]

parseOperand' "mm" = [Lmm]
parseOperand' "x" = [LXmm]
parseOperand' "xmm" = [LXmm]
parseOperand' "ymm" = [LYmm]
parseOperand' "(x)mm" = [LXmm, Lmm]

parseOperand' "cl" = [LCl]

parseOperand' "i" = [LImm]
parseOperand' "1" = [LImm]

parseOperand' _ = []

{- This restructure function takes a list of lists like
   [ [1,2,3,4] , [5,6], 7 ]
 
   and turns it into
   [ 1, 5, 7]
   [ 1, 6, 7]
   [ 2, 5, 7]
   [ 2, 6, 7]
   [ 3, 5, 7]
   [ 3, 6, 7]
   [ 4, 5, 7]
   [ 4, 6, 7] -}
restructure :: [[a]] -> [[a]]
restructure x = restructure' [] x

{- This assumes the first argument of the list is already determined,
   e.g.
    retructure [1] [[5,6], [7]] =
      [1, 5, 7]
      [1, 6, 7] -}
restructure' :: [a] -> [[a]] -> [[a]]
restructure' start [] = [start]
restructure' start (xs:xss) = let funs = (map (\t -> restructure' (start++[t])) xs) in
                                concat $  map (\f -> (f xss)) funs

readMaybe:: (Read a) => String -> Maybe a
readMaybe s =  case [x | (x,t) <- reads s, ("","") <- lex t] of
                    [x] -> Just x
                    _   -> Nothing

latency_for_instruction:: [LatencyRecord] -> Instr -> Int
latency_for_instruction lt i = 1

latency_lookup :: String -> [LatencyRecord]
latency_lookup file = let rows = map (splitOn ":") (lines file) 
                          records = concat (map processRow rows) in
                            records

