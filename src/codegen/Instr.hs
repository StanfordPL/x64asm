module Instr 
	( Instr(..)
	, parse_instrs
	, enum
	, arity
	, operand_widths
	, operand_types
	, operand_mods
	, mem_offset
	, cond_jump
	, uncond_jump
	, jump
	) where

import Data.Char
import Data.List.Split

-- Instruction Row Type
data Instr = 
  Instr { att             :: String   -- g++ mneumonic 
        , prefix          :: [String] -- prefix bytes
        , rex             :: String   -- operand-independent rex prefix byte
        , opcode          :: [String] -- opcode bytes
        , reg_code        :: Bool     -- opcode modified by operand?
        , reg_field       :: String   -- register field byte (follows opcode)
        , rm_operand      :: Bool     -- uses an rm operand?
        , rm_offset       :: Int      -- index for the rm operand
        , operands        :: [String]
        , implicit_reads  :: [String]
        , implicit_writes :: [String]
        , cond_read       :: [String]
        , cond_write      :: [String]
        , cond_undef      :: [String]
        , mem_size_or     :: Bool
        } deriving (Show)

-- Remove leading/trailing whitespace
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace
     
-- Read Instructions		
read_instrs :: String -> [Instr]
read_instrs s = map format $ rows
    where rows = map (splitOn ":") $ filter (\x -> ':' `elem` x) (lines s)
          format (a:p:o:rc:rf:os:ir:iw:cr:cw:cu:[]) = 
            (Instr (trim a) 
                   (words p) [] (words o) ((trim rc) == "y") (trim rf) False 0
                   (words os) 
                   (words ir) (words iw) 
                   (to_flag cr) (to_flag cw) (to_flag cu) 
                   False) 
          format _ = error $ "Unable to parse row"
          to_flag cs = map (++ "F") $ (words cs)

-- 16-bit operands require a 66 override prefix
add_66_prefix :: Instr -> Instr
add_66_prefix (Instr a p r o rc rf rm rmo os ir iw cr cw cu mo) =
    case os of
      ("16":"M":_)  -> (Instr a (p++["66"]) r o rc rf rm rmo os ir iw cr cw cu mo)
      ("16":"R":_)  -> (Instr a (p++["66"]) r o rc rf rm rmo os ir iw cr cw cu mo)
      ("16":"RM":_) -> (Instr a (p++["66"]) r o rc rf rm rmo os ir iw cr cw cu mo)
      _             -> (Instr a p           r o rc rf rm rmo os ir iw cr cw cu mo)

-- 64-bit operands require a mandator rex.w prefix
add_rexw_prefix :: Instr -> Instr
add_rexw_prefix (Instr a p _ o rc rf rm rmo os ir iw cr cw cu mo) =
  case os of
    ("64":"M":_)  -> (Instr a p "48" o rc rf rm rmo os ir iw cr cw cu mo)
    ("64":"R":_)  -> (Instr a p "48" o rc rf rm rmo os ir iw cr cw cu mo)
    ("64":"RM":_) -> (Instr a p "48" o rc rf rm rmo os ir iw cr cw cu mo)
    _            -> (Instr a p ""   o rc rf rm rmo os ir iw cr cw cu mo)

-- We need to distinguish the possibility and position of r/m operands
add_rm_info :: Instr -> Instr
add_rm_info (Instr a p r o rc rf _ _ os ir iw cr cw cu mo) =
  case os of 
    (_:"RM":_)       -> (Instr a p r o rc rf True  0 os ir iw cr cw cu mo)
    (_:"SM":_)       -> (Instr a p r o rc rf True  0 os ir iw cr cw cu mo)
    (_:"XM":_)       -> (Instr a p r o rc rf True  0 os ir iw cr cw cu mo)
    (_:_:_:_:"RM":_) -> (Instr a p r o rc rf True  1 os ir iw cr cw cu mo)
    (_:_:_:_:"SM":_) -> (Instr a p r o rc rf True  1 os ir iw cr cw cu mo)
    (_:_:_:_:"XM":_) -> (Instr a p r o rc rf True  1 os ir iw cr cw cu mo)
    _                -> (Instr a p r o rc rf False 0 os ir iw cr cw cu mo)

-- Expand instructions operands
expand_operands :: String -> String -> String -> Instr -> [Instr]
expand_operands key val1 val2 (Instr a p r o rc rf rm rmo os ir iw cr cw cu mo) =
    case (elem key os) of 
      True  -> (Instr a p r o rc rf rm rmo (repl key val1 os) ir iw cr cw cu mo) :
               (Instr a p r o rc rf rm rmo (repl key val2 os) ir iw cr cw cu mo) : []
      False -> (Instr a p r o rc rf rm rmo os                 ir iw cr cw cu mo) : []
    where repl k v os = map (repl' k v) os
          repl' k v o = case (o == k) of 
            True  -> v
            False -> o

-- Expand mem override
expand_mem_or :: Instr -> [Instr]
expand_mem_or (Instr a p r o rc rf rm rmo os ir iw cr cw cu _) =
  (Instr a p r o rc rf rm rmo os ir iw cr cw cu False) :
  case (elem "M" os) of
    True  -> (Instr a (insert p) r o rc rf rm rmo os ir iw cr cw cu True) : []
    False -> []								
    where insert [] = "67":[]
          insert ps = (head ps):["67"]++(tail ps)


-- Applies each of the above functions in appropriate turn					
expand_instr :: Instr -> [Instr]
expand_instr i = 
  --concat $ map expand_mem_or $
  concat $ map (expand_operands "XM" "X" "M") $
  concat $ map (expand_operands "SM" "S" "M") $
  (expand_operands "RM" "R" "M") $ 
  add_rm_info $ add_rexw_prefix $ add_66_prefix i  

-- Read and auto-complete
parse_instrs :: String -> [Instr]
parse_instrs s = concat $ map expand_instr $ read_instrs s

-- Generates a unique enum value for an instruction
enum :: Instr -> String
enum instr = (up (att instr)) ++ 
             (arg (operands instr)) ++ 
             (rm_suff (rm_operand instr) (rm_offset instr)) ++
             (mo_suff (mem_size_or instr))
    where up xs = (map toUpper xs)
          arg (w:t:_:xs) = "_" ++ w ++ (up t) ++ (arg xs)
          arg _ = ""					
          rm_suff True i = "_RM" ++ (show i)
          rm_suff _ _    = ""
          mo_suff True = "_OR"
          mo_suff _    = ""

-- Returns number of operands
arity :: Instr -> Int
arity instr = (length (operands instr)) `div` 3

-- Unpacks operand widths
operand_widths :: Instr -> [String]
operand_widths instr = ow $ operands instr
    where ow (w:_:_:os) = w:(ow os)
          ow _ = []

-- Unpacks operand types
operand_types :: Instr -> [String]
operand_types instr = ot $ operands instr
    where ot (_:t:_:os) = t:(ot os)
          ot _ = []

-- Unpacks operand modifiers
operand_mods :: Instr -> [String]
operand_mods instr = om $ operands instr
    where om (_:_:m:os) = m:(om os)
          om _ = []

-- Index of memory operand
mem_offset :: Instr -> (Maybe Int)
mem_offset instr = mo (operand_types instr) 0
    where mo ("M":_) i = (Just i)
          mo (_:os)  i = mo os (i+1)
          mo []      _ = Nothing

-- Is the instruction a conditional jump?
cond_jump :: Instr -> Bool
cond_jump i = cj $ att i
    where cj "jae" = True
          cj "ja"  = True
          cj "jbe" = True
          cj "jb"  = True
          cj "je"  = True
          cj "jge" = True
          cj "jg"  = True
          cj "jle" = True
          cj "jl"  = True
          cj "jne" = True
          cj "jns" = True
          cj "js"  = True
          cj _     = False

-- Is the instruction an unconditional jump?
uncond_jump :: Instr -> Bool
uncond_jump i = uj $ att i
    where uj "jmp" = True
          uj _     = False

-- Is the instruction a jump?
jump :: Instr -> Bool
jump i = (cond_jump) i || (uncond_jump i)
