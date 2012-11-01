module Latency
  ( latency_for_instruction,
    latency_lookup,
    LatencyOperand(..),
    LatencyRecord(..) ) where

import Data.List.Split
import Data.Char
import Instr

data LatencyOperand = LReg (Maybe String) | LMem (Maybe String) | LImm | LCl | LUnknown | LAx 
                        | LXmm | Lmm | LYmm | LXmm0 | LST0 | Lmmx | LLabel deriving (Show)
data LatencyRecord = Mk_lr String [LatencyOperand] Float deriving (Show)

operands_match::[(String,String,String)] -> [LatencyOperand] -> Bool
operands_match ios los = if length ios == length los then
                            if length ios == 0 then True else
                            foldl1 (&&) (map (\(a,b) -> operands_match' a b) (zip ios los))
                         else
                            False

operands_match'::(String,String,String) -> LatencyOperand -> Bool
operands_match' (_,"R",_) (LReg Nothing) = True
operands_match' (w,"R",_) (LReg (Just w')) = (w == w')
operands_match' (_,"M",_) (LMem Nothing) = True
operands_match' (w,"M",_) (LMem (Just w')) = (w == w')
operands_match' (_,"S",_) (LXmm)         = True
operands_match' (_,"Y",_) (LYmm)         = True
operands_match' (_,"X",_) (Lmmx)         = True
operands_match' (_,"F",_) (Lmm)          = True
operands_match' (_,"AL",_) (LReg (Just "8"))   = True
operands_match' (_,"AX",_) (LReg (Just"16"))   = True
operands_match' (_,"AX",_) (LAx)         = True
operands_match' (_,"EAX",_) (LReg (Just "32"))  = True
operands_match' (_,"RAX",_) (LReg (Just "64"))  = True
operands_match' (_,"CL",_) (LCl)         = True
operands_match' (_,"ST0",_) (LST0)       = True
operands_match' (_,"XMM0",_) (LXmm0)     = True
operands_match' (_,"L",_) (LLabel)       = True
operands_match' _ _                      = False


get_att :: LatencyRecord -> String
get_att (Mk_lr s _ _) = s

get_types :: LatencyRecord -> [LatencyOperand]
get_types (Mk_lr _ ts _) = ts

get_latency :: LatencyRecord -> Float
get_latency (Mk_lr _ _ f) = f

cleanup :: [String] -> [String]
cleanup [] = []
cleanup (x:xs) = (cleanup' x) ++ cleanup xs

--todo: handle () and /
cleanup' :: String -> [String]
cleanup' str = [str]

processRow::[String] -> [LatencyRecord]
processRow list = let opcodes = words $ map toLower (list !! 0)
                      opcodes' = cleanup opcodes'
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
parseOperands ls = if ls == "" then [[]]
                     else restructure $ map parseOperand (splitOn "," ls)

parseOperand :: String -> [LatencyOperand]
parseOperand s = concat $ map parseOperand' $ splitOn "/" s

parseOperand' :: String -> [LatencyOperand]
parseOperand' "r" = [LReg Nothing]
parseOperand' "r8" = [LReg (Just "8")]
parseOperand' "r16" = [LReg (Just "16")]
parseOperand' "r32" = [LReg (Just "32")]
parseOperand' "r64" = [LReg (Just "64")]
parseOperand' "r80" = [LReg (Just "80")]

parseOperand' "m" = [LMem Nothing]
parseOperand' "m8" = [LMem (Just "8")]
parseOperand' "m16" = [LMem (Just "16")]
parseOperand' "m32" = [LMem (Just "32")]
parseOperand' "m64" = [LMem (Just "64")]
parseOperand' "m80" = [LMem (Just "80")]
parseOperand' "m128" = [LMem (Just "64")]
parseOperand' "m256" = [LMem (Just "64")]

parseOperand' "mm" = [Lmm]
parseOperand' "x" = [LXmm]
parseOperand' "xmm" = [LXmm]
parseOperand' "xmm0" = [LXmm0]
parseOperand' "ymm" = [LYmm]
parseOperand' "(x)mm" = [LXmm, Lmm]

parseOperand' "short" = [LLabel]
parseOperand' "near" = [LLabel]

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
latency_for_instruction lt (Instr att _ _ _ _ _ _ ops _ _ _ _ _) =
  let table_for_i = filter (\lr -> (elem att
                                     (map (\t -> ((get_att lr) ++ t)) 
                                          ["b", "w", "l", "q", "s", "d", ""]))   &&
                                     ( operands_match ops (get_types lr) ) ) lt 
      latencies = map get_latency table_for_i in
  if length latencies > 0 then
    round $ (*) 10 ((sum latencies)/(fromIntegral $ length latencies))
  else
    latency_for_instruction_give_up lt att



--this is the version used if we cannot find an exact match for the operands
latency_for_instruction_give_up lt att =
  let table_for_i = filter (\lr -> elem att
                                   (map (\t -> ((get_att lr) ++ t)) 
                                        ["b","w","l","q","s", "d", ""])  ) lt
      latencies = map get_latency table_for_i in
  if length latencies > 0 then
    round $ (*) 10 ((sum latencies)/(fromIntegral $ length latencies))
  else
    20

latency_lookup :: String -> [LatencyRecord]
latency_lookup file = let rows = map (splitOn ":") (lines file) 
                          records = concat (map processRow rows) in
                            records

