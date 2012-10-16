module Instr 
	( Instr(..)
	, parse_instrs
	, operand_widths
	, operand_types
	, operand_mods
	, to_enum
	, to_type
	, to_type_enum
	, to_mod_enum
	, arity
	, cond_jump
	, uncond_jump
	, jump
	, ret
	, mem_index
	, first_read
	, num_writes

	, to_type_enum_DEPRECATED
	, to_width_enum_DEPRECATED
	) where

import Data.Char
import Data.List
import Data.List.Split

-- Instruction Row Type
data Instr = 
  Instr { att             :: String   -- g++ mneumonic 
        , prefix          :: [String] -- prefix bytes
        , rex             :: String   -- operand-independent rex prefix byte
        , opcode          :: [String] -- opcode bytes
        , reg_code        :: Bool     -- opcode modified by operand?
        , reg_field       :: String   -- register field byte (follows opcode)
        , flipped         :: Bool     -- does the r/m operand come second?
        , operands        :: [(String,String,String)]
        , implicit_reads  :: [String]
        , implicit_writes :: [String]
        , cond_read       :: [String]
        , cond_write      :: [String]
        , cond_undef      :: [String]
        } deriving (Show)

-- Remove leading/trailing whitespace
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace
     
-- Upper/Lowercase strings
up :: String -> String
up s = map toUpper s

low :: String -> String
low s = map toLower s

-- Groups strings into tuples, three at a time
gather :: [String] -> [(String,String,String)]
gather (x:y:z:xs) = (x,y,z):(gather xs)
gather [] = []
gather _ = error "Unable to parse operands!"

-- Appends an F to a flag string and lower cases
to_flags :: String -> [String]
to_flags s = map low $ map (++"F") $ words s

-- Lower cases register names
to_regs :: String -> [String]
to_regs s = map low $ words s

-- Read Instructions		
read_instrs :: String -> [Instr]
read_instrs s = map format $ rows
    where rows = map (splitOn ":") $ filter (\x -> ':' `elem` x) (lines s)
          format (a:p:o:rc:rf:os:ir:iw:cr:cw:cu:[]) = 
            (Instr (trim a) 
                   (words p) [] (words o) ((trim rc) == "y") (trim rf) False
                   (gather (words os))
                   (to_regs ir) (to_regs iw) 
                   (to_flags cr) (to_flags cw) (to_flags cu) 
                   ) 
          format _ = error $ "Unable to parse row"

-- Most 16-bit operands require a 66 override prefix
add_66_prefix :: Instr -> Instr
add_66_prefix (Instr a p r o rc rf f os ir iw cr cw cu) =
    case a of
        "fiaddl" -> (Instr a p r o rc rf f os ir iw cr cw cu)
        "fiadds" -> (Instr a p r o rc rf f os ir iw cr cw cu)
        _ -> case os of
              ("16","M",_):_  -> (Instr a ("66":p) r o rc rf f os ir iw cr cw cu)
              ("16","R",_):_  -> (Instr a ("66":p) r o rc rf f os ir iw cr cw cu)
              ("16","RM",_):_ -> (Instr a ("66":p) r o rc rf f os ir iw cr cw cu)
              ("16","AX",_):_ -> (Instr a ("66":p) r o rc rf f os ir iw cr cw cu)
              ("16","CX",_):_ -> error "Does this ever happen?  CX Target?"
              _ -> (Instr a p r o rc rf f os ir iw cr cw cu)

-- Most 64-bit operands require a mandator rex.w prefix
add_rexw_prefix :: Instr -> Instr
add_rexw_prefix (Instr a p _ o rc rf f os ir iw cr cw cu) =
  case os of
    ("64","M",_):_   -> case a of
                        "faddl" -> (Instr a p "" o rc rf f os ir iw cr cw cu)
                        _ -> (Instr a p "48" o rc rf f os ir iw cr cw cu)
    ("64","O",_):_   -> (Instr a p "48" o rc rf f os ir iw cr cw cu)
    ("64","RM",_):_  -> case a of 
                        "pushq" -> (Instr a p "" o rc rf f os ir iw cr cw cu)
                        "popq"  -> (Instr a p "" o rc rf f os ir iw cr cw cu)
                        _ -> (Instr a p "48" o rc rf f os ir iw cr cw cu)
    ("64","R",_):_   -> case a of 
                        "pushq" -> (Instr a p "" o rc rf f os ir iw cr cw cu)
                        "popq"  -> (Instr a p "" o rc rf f os ir iw cr cw cu)
                        _ -> (Instr a p "48" o rc rf f os ir iw cr cw cu)
    ("64","RAX",_):_ -> (Instr a p "48" o rc rf f os ir iw cr cw cu)
    _ -> (Instr a p "" o rc rf f os ir iw cr cw cu)

-- Record instructions with r/m operands as second arguments
add_rm_info :: Instr -> Instr
add_rm_info (Instr a p r o rc rf _ os ir iw cr cw cu) =
  let (_,ts,_) = unzip3 os in
  case ts of 
    _:"RM":_ -> (Instr a p r o rc rf True os ir iw cr cw cu)
    _:"SM":_ -> (Instr a p r o rc rf True os ir iw cr cw cu)
    _:"XM":_ -> (Instr a p r o rc rf True os ir iw cr cw cu)
    _ -> (Instr a p r o rc rf False os ir iw cr cw cu)

-- Expand instructions operands
expand_operands :: String -> String -> String -> Instr -> [Instr]
expand_operands key val1 val2 (Instr a p r o rc rf f os ir iw cr cw cu) =
    let (_,ts,_) = unzip3 os in
    case (elem key ts) of 
      True  -> (Instr a p r o rc rf f (repl key val1 os) ir iw cr cw cu) :
               (Instr a p r o rc rf f (repl key val2 os) ir iw cr cw cu) : []
      False -> (Instr a p r o rc rf f os                 ir iw cr cw cu) : []
    where repl k v os = map (repl' k v) os
          repl' k v (w,t,m) = case (t == k) of 
            True  -> (w,v,m) 
            False -> (w,t,m)

-- Applies each of the above functions in appropriate turn					
expand_instr :: Instr -> [Instr]
expand_instr i = 
  concat $ map (expand_operands "XM" "X" "M") $
  concat $ map (expand_operands "SM" "S" "M") $
  (expand_operands "RM" "R" "M") $ 
  add_rm_info $ add_rexw_prefix $ add_66_prefix i  

-- Selects the prefered implementation of a redundant instruction
-- Note that this leaves label_defn as the first element
remove_redundant :: [Instr] -> [Instr]
remove_redundant is = map keep $ groupBy eq $ sortBy lt is
    where lt i1 i2 = compare (to_enum i1) (to_enum i2)
          eq i1 i2 = (to_enum i1) == (to_enum i2)
          keep = head

-- Read and auto-complete
parse_instrs :: String -> [Instr]
parse_instrs s = let is = concat $ map expand_instr $ read_instrs s in
  (head is) : (remove_redundant $ tail is)

-- Unpacks operand widths
operand_widths :: Instr -> [String]
operand_widths instr = let (x,_,_) = unzip3 (operands instr) in x

-- Unpacks operand types
operand_types :: Instr -> [String]
operand_types instr = let (_,x,_) = unzip3 (operands instr) in x

-- Unpacks operand modifiers
operand_mods :: Instr -> [String]
operand_mods instr = let (_,_,x) = unzip3 (operands instr) in x

-- Generates a unique enum value for an instruction
to_enum :: Instr -> String
to_enum instr = (up (att instr)) ++ (arg (operands instr))
    where arg ((w,t,_):xs) = "_" ++ w ++ (up t) ++ (arg xs)
          arg _ = ""					

-- Generates the type encoded by an operand column
to_type:: (String,String,String) -> String
to_type (w,"M",_) = "M" ++ w
to_type (_,"F",_) = "St"
to_type (w,"R",_) = "R" ++ w
to_type (w,"I",_) = "Imm" ++ w
to_type (_,"L",_) = "Label"
to_type (_,"X",_) = "Mm"
to_type (w,"O",_) = "Moffs" ++ w
to_type (_,"S",_) = "Xmm"
to_type (_,"Y",_) = "Ymm"
to_type (_,"AL",_) = "Al"
to_type (_,"AX",_) = "Ax"
to_type (_,"EAX",_) = "Eax"
to_type (_,"RAX",_) = "Rax"
to_type (_,"CL",_) = "Cl"
to_type (_,"ST0",_) = "St0"
to_type (_,_,_) = error "Unrecognized type!"

-- Generates the type_enum encoded by an operand column
to_type_enum :: (String,String,String) -> String
to_type_enum (w,"M",_) = "M_" ++ w
to_type_enum (_,"F",_) = "ST"
to_type_enum (w,"R",_) = "R_" ++ w
to_type_enum (w,"I",_) = "IMM_" ++ w
to_type_enum (_,"L",_) = "LABEL"
to_type_enum (_,"X",_) = "MM"
to_type_enum (w,"O",_) = "MOFFS_" ++ w
to_type_enum (_,"S",_) = "XMM"
to_type_enum (_,"Y",_) = "YMM"
to_type_enum (_,"AL",_) = "AL"
to_type_enum (_,"AX",_) = "AX"
to_type_enum (_,"EAX",_) = "EAX"
to_type_enum (_,"RAX",_) = "RAX"
to_type_enum (_,"CL",_) = "CL"
to_type_enum (_,"ST0",_) = "ST0"
to_type_enum (_,_,_) = error "Unrecognized type!"

-- Generates the modifier enum encoded by an operand column
to_mod_enum :: (String,String,String) -> String
to_mod_enum (_,_,"R") = "READ"
to_mod_enum (_,_,"W") = "WRITE"
to_mod_enum (_,_,"X") = "READ_WRITE"
to_mod_enum (_,_,"N") = "NONE"
to_mod_enum _ = error "Unrecognized modifier!"

-- Returns number of operands
arity :: Instr -> Int
arity instr = (length (operands instr))

-- Is the instruction a conditional jump?
cond_jump :: Instr -> Bool
cond_jump i = cj $ att i
    where cj "ja"  = True
          cj "jae" = True
          cj "jb"  = True
          cj "jbe" = True
          cj "je"  = True
          cj "jg"  = True
          cj "jge" = True
          cj "jl"  = True
          cj "jle" = True
          cj "jne" = True
          cj "jno" = True
          cj "jnp" = True
          cj "jns" = True
          cj "jo"  = True
          cj "jp"  = True
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

-- Is the instruction a ret?
ret :: Instr -> Bool
ret i = r $ att i
    where r "retq" = True
          r _ = False

-- What index does memory appear at?
mem_index :: Instr -> Int
mem_index i = mi (operand_types i) 0
    where mi ("M":_) i = i
          mi ("O":_) i = i
          mi (_:xs) i = mi xs (i+1)
          mi [] _ = 3

-- What is the first operand that gets read?
first_read :: Instr -> Int
first_read i = fr (operands i) 0
    where fr ((_,"M",_):_) i = i
          fr ((_,_,"R"):_) i = i
          fr ((_,"X",_):_) i = i
          fr (_:xs) i = fr xs (i+1)
          fr [] _ = 3

-- What is the first non-write operand?
num_writes :: Instr -> Int
num_writes i = nw (operand_mods i) 0
    where nw ("W":xs) i = nw xs (i+1)
          nw ("X":xs) i = nw xs (i+1)
          nw _ i = i



-- DEPRECATED METHODS
-- Used by att parser

to_type_enum_DEPRECATED :: (String,String,String) -> String
to_type_enum_DEPRECATED (_,"M",_) = "ADDR"
to_type_enum_DEPRECATED (_,"F",_) = "FP_REG"
to_type_enum_DEPRECATED (_,"R",_) = "GP_REG"
to_type_enum_DEPRECATED (_,"I",_) = "IMM"
to_type_enum_DEPRECATED (_,"L",_) = "LABEL"
to_type_enum_DEPRECATED (_,"X",_) = "MMX_REG"
to_type_enum_DEPRECATED (_,"O",_) = "OFFSET"
to_type_enum_DEPRECATED (_,"S",_) = "XMM_REG"
to_type_enum_DEPRECATED (_,"AL",_) = "RAX_ONLY"
to_type_enum_DEPRECATED (_,"AX",_) = "RAX_ONLY"
to_type_enum_DEPRECATED (_,"EAX",_) = "RAX_ONLY"
to_type_enum_DEPRECATED (_,"RAX",_) = "RAX_ONLY"
to_type_enum_DEPRECATED (_,"CL",_) = "RCX_ONLY"
to_type_enum_DEPRECATED (_,"ST0",_) = "ST0_ONLY"
to_type_enum_DEPRECATED _ = error "Unrecognized type!"

width :: String -> String
width "8"   = "LOW"					
width "16"  = "WORD"				
width "32"  = "DOUBLE"					
width "64"  = "QUAD"					
width "128" = "OCT"					
width _     = "BIT_WIDTH_VAL_NULL"

to_width_enum_DEPRECATED :: (String,String,String) -> String
to_width_enum_DEPRECATED (w,"M",_) = width w
to_width_enum_DEPRECATED (_,"F",_) = "QUAD"
to_width_enum_DEPRECATED (w,"R",_) = width w
to_width_enum_DEPRECATED (w,"I",_) = width w
to_width_enum_DEPRECATED (_,"L",_) = "FIXED"
to_width_enum_DEPRECATED (_,"X",_) = "QUAD"
to_width_enum_DEPRECATED (_,"O",_) = "QUAD"
to_width_enum_DEPRECATED (_,"S",_) = "OCT"
to_width_enum_DEPRECATED (_,"AL",_) = "LOW"
to_width_enum_DEPRECATED (_,"AX",_) = "WORD"
to_width_enum_DEPRECATED (_,"EAX",_) = "DOUBLE"
to_width_enum_DEPRECATED (_,"RAX",_) = "QUAD"
to_width_enum_DEPRECATED (_,"CL",_) = "LOW"
to_width_enum_DEPRECATED (_,"ST0",_) = "QUAD"
to_width_enum_DEPRECATED _ = error "Unrecognized type!"
