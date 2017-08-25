{-
Copyright 2013-2015 Stanford University

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

import Data.Char
import Data.List
import Data.List.Split
import System.Environment
import Text.Regex
import Text.Regex.TDFA

--------------------------------------------------------------------------------
-- Row Data Types 
-- Corresponds to the rows in x86.csv
--------------------------------------------------------------------------------

-- Instruction Row Type
data Instr =
  Instr { opcode      :: String -- Regular expression-ish hex encoding
        , instruction :: String -- Regular expression-ish name and type
        , op_en       :: String -- Operand type/position tags
        , property    :: String -- Operand read/write/undef properties
        , imp_read    :: String -- Implicit read set
        , imp_write   :: String -- Implicit write set
        , imp_undef   :: String -- Implicit undef set
        , useful      :: String -- Is this a useful system instruction?
        , protected   :: String -- Is this a protected system instruction?
        , mode64      :: String -- Is this instruction valid in 64-bit mode?
        , mode32      :: String -- Is this instruction valid in 32-bit mode?
        , flag        :: String -- CPUID flag
        , att         :: String -- att mnemonic (per gcc)
        , pref        :: String -- Prefer this over equally valid alternative
        , description :: String -- Intel manual description
        } deriving (Show)

--------------------------------------------------------------------------------
-- Common Helper Methods
--------------------------------------------------------------------------------

-- String transforms
--------------------------------------------------------------------------------

-- Remove leading/trailing whitespace
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- To lower case
low :: String -> String
low s = map toLower s

-- To upper case
up :: String -> String
up s = map toUpper s

-- Replaces whitespace by underscore
kill_ws :: Char -> Char
kill_ws c
  | isSpace c = '_'
  | otherwise = c

-- Replace whitespace by underscore
to_id :: String -> String
to_id s = map kill_ws s

-- Instruction modification
--------------------------------------------------------------------------------

-- Replace all occurrences of an operand
repl_op :: Instr -> String -> String -> Instr
repl_op i op val = i{instruction=inst'}
  where inst = instruction i
        inst' = subRegex (mkRegex op) inst (val)

-- Replace first occurrence of an operand
repl_first_op :: Instr -> String -> String -> Instr
repl_first_op i op val = i{instruction=inst'}
  where inst' = (raw_mnemonic i) ++ " " ++ (intercalate ", " ops)
        os = operands i
        ops = case findIndex (==op) os of
                   (Just idx) -> (take idx os) ++ [val] ++ (drop (idx+1) os)
                   Nothing -> os

-- Table formatting
--------------------------------------------------------------------------------

-- Transforms a list of instructions into a comma separated table
to_table :: [Instr] -> (Instr -> String) -> String
to_table is f = intercalate "\n" $ map elem is
  where elem i = ", " ++ (f i) ++ " // " ++ instruction i

-- Opcode related
--------------------------------------------------------------------------------

-- Extract opcode terms
opcode_terms :: Instr -> [String]
opcode_terms i = splitOn " " (opcode i)

-- Is this a prefix (ie: a byte that must appear before the REX prefix byte)?
is_prefix :: String -> Bool
is_prefix "PREF.66+" = True
is_prefix "66"       = True -- Not the same as PREF.66+; different semantics
is_prefix "REX.W+"   = True
is_prefix "REX.R+"   = True
is_prefix "REX+"     = True
is_prefix "F2"       = True -- Not the same as REP; different semantics
is_prefix "F3"       = True -- Not the same as REP; different semantics
is_prefix "9B"       = True -- The FWAIT prefix
is_prefix ('V':_)    = True -- VEX Prefixes all start with a V
is_prefix _ = False

-- Is this a suffix?
is_suffix :: String -> Bool
is_suffix ('+':_) = True
is_suffix ('/':_) = True
is_suffix ('i':_) = True
is_suffix ('c':_) = True
is_suffix _ = False

-- Is this a mod r/m digit?
is_digit :: String -> Bool
is_digit ('/':d:_) = isDigit d
is_digit _ = False

-- Extracts VEX terms
vex_terms :: Instr -> [String]
vex_terms i = splitOn "." $ head $ opcode_terms i

-- Extracts prefix bytes
opcode_prefix :: Instr -> [String]
opcode_prefix i = takeWhile is_prefix $ opcode_terms i

-- Extracts opcode bytes 
opcode_bytes :: Instr -> [String]
opcode_bytes i = takeWhile not_suffix $ dropWhile is_prefix $ ts 
  where not_suffix t = not $ is_suffix t
        ts = opcode_terms i

-- Extracts suffix bytes
opcode_suffix :: Instr -> [String]
opcode_suffix i = dropWhile not_suffix $ dropWhile is_prefix $ ts
  where not_suffix t = not $ is_suffix t
        ts = opcode_terms i

-- Instruction related
--------------------------------------------------------------------------------

-- Extract raw mnemonic
raw_mnemonic :: Instr -> String
raw_mnemonic i = head $ words $ instruction i

-- Returns true for conditional jump instructions
is_cond_jump :: Instr -> Bool
is_cond_jump i = let mn = raw_mnemonic i in
  head mn == 'J' && mn /= "JMP"

-- Returns true for unconditional jump instructions
is_uncond_jump :: Instr -> Bool
is_uncond_jump i = raw_mnemonic i == "JMP"

-- Extract arity
arity :: Instr -> Int
arity i = length $ operands i

-- Extract operands 
operands :: Instr -> [String]
operands i = let x = (splitOn ",") $ concat $ tail $ words (instruction i) in
  filter (\o -> o /= "") x

-- Returns true for register operands
reg_op :: String -> Bool
reg_op "rh"  = True
reg_op "r8"  = True
reg_op "r16" = True
reg_op "r32" = True
reg_op "r64" = True
reg_op "AL"  = True
reg_op "CL"  = True
reg_op "AX"  = True
reg_op "DX"  = True
reg_op "EAX" = True
reg_op "RAX" = True
reg_op _ = False

-- Returns the number of bits in a type
op_width :: String -> Int
op_width "rh"       = 8
op_width "r8"       = 8
op_width "r16"      = 16
op_width "r32"      = 32
op_width "r64"      = 64
op_width "AL"       = 8
op_width "CL"       = 8
op_width "AX"       = 16
op_width "DX"       = 16
op_width "EAX"      = 32
op_width "RAX"      = 64
op_width "m8"       = 8
op_width "m16"      = 16
op_width "m32"      = 32
op_width "m64"      = 64
op_width "m128"     = 128
op_width "m256"     = 256
op_width "m16:16"   = 0
op_width "m16:32"   = 0
op_width "m16:64"   = 0
op_width "m16int"   = 16
op_width "m32int"   = 32
op_width "m64int"   = 64
op_width "m80bcd"   = 80
op_width "m32fp"    = 32
op_width "m64fp"    = 64
op_width "m80fp"    = 80
op_width "m2byte"   = 2
op_width "m28byte"  = 28
op_width "m108byte" = 108
op_width "m512byte" = 512
op_width "imm8"     = 8
op_width "imm16"    = 16
op_width "imm32"    = 32
op_width "imm64"    = 64
op_width "0"        = 0
op_width "1"        = 0
op_width "3"        = 0
op_width "mm"       = 64
op_width "xmm"      = 128
op_width "<XMM0>"   = 128
op_width "ymm"      = 256
op_width "ST"       = 0
op_width "ST(i)"    = 0
op_width "rel8"     = 8
op_width "rel32"    = 32
op_width "moffs8"   = 8
op_width "moffs16"  = 16
op_width "moffs32"  = 32
op_width "moffs64"  = 64
op_width "Sreg"     = 0
op_width "FS"       = 0
op_width "GS"       = 0
op_width "p66"      = 0
op_width "pw"       = 0
op_width "far"      = 0
op_width "label32"  = 0
op_width "label8"   = 0
op_width "hint"     = 0

-- Returns true for 32-bit register operands
reg32_op :: String -> Bool
reg32_op "r32" = True
reg32_op "EAX" = True
reg32_op _ = False

-- Returns true for memory operands
mem_op :: String -> Bool
mem_op "m8"       = True
mem_op "m16"      = True
mem_op "m32"      = True
mem_op "m64"      = True
mem_op "m128"     = True
mem_op "m256"     = True
mem_op "m16:16"   = True
mem_op "m16:32"   = True
mem_op "m16:64"   = True
mem_op "m16int"   = True
mem_op "m32int"   = True
mem_op "m64int"   = True
mem_op "m80bcd"   = True
mem_op "m32fp"    = True
mem_op "m64fp"    = True
mem_op "m80fp"    = True
mem_op "m2byte"   = True
mem_op "m28byte"  = True
mem_op "m108byte" = True
mem_op "m512byte" = True
mem_op _ = False

-- Returns true for immediate operands
imm_op :: String -> Bool
imm_op "imm8" = True
imm_op "imm16" = True
imm_op "imm32" = True
imm_op "imm64" = True
imm_op _ = False

-- Returns true for label operands
label_op :: String -> Bool
label_op "label8" = True
label_op "label32" = True
label_op _ = False

-- Returns true for moffs operands
moffs_op :: String -> Bool
moffs_op "moffs8" = True
moffs_op "moffs16" = True
moffs_op "moffs32" = True
moffs_op "moffs64" = True
moffs_op _ = False

-- Returns true for rel operands
rel_op :: String -> Bool
rel_op "rel8" = True
rel_op "rel32" = True
rel_op _ = False

-- Returns true for sreg operands
sreg_op :: String -> Bool
sreg_op "Sreg" = True
sreg_op "FS"   = True
sreg_op "GS"   = True
sreg_op _      = False

-- Returns true for st operands
st_op :: String -> Bool
st_op "ST"    = True
st_op "ST(i)" = True
st_op _       = False

-- Returns true for xmm operands
xmm_op :: String -> Bool
xmm_op "xmm"    = True
xmm_op "<XMM0>" = True
xmm_op _        = False

-- Transform operand into c++ type
op2type :: String -> String
op2type "rh"       = "Rh"
op2type "r8"       = "R8"
op2type "r16"      = "R16"
op2type "r32"      = "R32"
op2type "r64"      = "R64"
op2type "AL"       = "Al"
op2type "CL"       = "Cl"
op2type "AX"       = "Ax"
op2type "DX"       = "Dx"
op2type "EAX"      = "Eax" 
op2type "RAX"      = "Rax"
op2type "m8"       = "M8"
op2type "m16"      = "M16"
op2type "m32"      = "M32"
op2type "m64"      = "M64"
op2type "m128"     = "M128"
op2type "m256"     = "M256"
op2type "m16:16"   = "FarPtr1616" 
op2type "m16:32"   = "FarPtr1632"
op2type "m16:64"   = "FarPtr1664"
op2type "m16int"   = "M16Int"
op2type "m32int"   = "M32Int"
op2type "m64int"   = "M64Int"
op2type "m80bcd"   = "M80Bcd" 
op2type "m32fp"    = "M32Fp"
op2type "m64fp"    = "M64Fp"
op2type "m80fp"    = "M80Fp"
op2type "m2byte"   = "M2Byte" 
op2type "m28byte"  = "M28Byte" 
op2type "m108byte" = "M108Byte" 
op2type "m512byte" = "M512Byte" 
op2type "imm8"     = "Imm8"
op2type "imm16"    = "Imm16"
op2type "imm32"    = "Imm32"
op2type "imm64"    = "Imm64"
op2type "0"        = "Zero"
op2type "1"        = "One"
op2type "3"        = "Three"
op2type "mm"       = "Mm"
op2type "xmm"      = "Xmm"
op2type "<XMM0>"   = "Xmm0"
op2type "ymm"      = "Ymm"
op2type "ST"       = "St0"
op2type "ST(i)"    = "St"
op2type "rel8"     = "Rel8"
op2type "rel32"    = "Rel32"
op2type "moffs8"   = "Moffs8"
op2type "moffs16"  = "Moffs16"
op2type "moffs32"  = "Moffs32"
op2type "moffs64"  = "Moffs64"
op2type "Sreg"     = "Sreg"
op2type "FS"       = "Fs"
op2type "GS"       = "Gs"
-- Below this point are operand types we have introduced
op2type "p66"      = "Pref66"
op2type "pw"       = "PrefRexW"
op2type "far"      = "Far"
op2type "label32"  = "Label"
op2type "label8"   = "Label"
op2type "hint"     = "Hint"
op2type o = error $ "Unrecognized operand type: \"" ++ o ++ "\""

-- Transform operand into type tag
op2tag :: String -> String
op2tag "rh"       = "RH"
op2tag "r8"       = "R_8"
op2tag "r16"      = "R_16"
op2tag "r32"      = "R_32"
op2tag "r64"      = "R_64"
op2tag "AL"       = "AL"
op2tag "CL"       = "CL"
op2tag "AX"       = "AX"
op2tag "DX"       = "DX"
op2tag "EAX"      = "EAX" 
op2tag "RAX"      = "RAX"
op2tag "m8"       = "M_8"
op2tag "m16"      = "M_16"
op2tag "m32"      = "M_32"
op2tag "m64"      = "M_64"
op2tag "m128"     = "M_128"
op2tag "m256"     = "M_256"
op2tag "m16:16"   = "FAR_PTR_16_16" 
op2tag "m16:32"   = "FAR_PTR_16_32"
op2tag "m16:64"   = "FAR_PTR_16_64"
op2tag "m16int"   = "M_16_INT"
op2tag "m32int"   = "M_32_INT"
op2tag "m64int"   = "M_64_INT"
op2tag "m80bcd"   = "M_80_BCD" 
op2tag "m32fp"    = "M_32_FP"
op2tag "m64fp"    = "M_64_FP"
op2tag "m80fp"    = "M_80_FP"
op2tag "m2byte"   = "M_2_BYTE" 
op2tag "m28byte"  = "M_28_BYTE" 
op2tag "m108byte" = "M_108_BYTE" 
op2tag "m512byte" = "M_512_BYTE" 
op2tag "imm8"     = "IMM_8"
op2tag "imm16"    = "IMM_16"
op2tag "imm32"    = "IMM_32"
op2tag "imm64"    = "IMM_64"
op2tag "0"        = "ZERO"
op2tag "1"        = "ONE"
op2tag "3"        = "THREE"
op2tag "mm"       = "MM"
op2tag "xmm"      = "XMM"
op2tag "<XMM0>"   = "XMM_0"
op2tag "ymm"      = "YMM"
op2tag "ST"       = "ST_0"
op2tag "ST(i)"    = "ST"
op2tag "rel8"     = "REL_8"
op2tag "rel32"    = "REL_32"
op2tag "moffs8"   = "MOFFS_8"
op2tag "moffs16"  = "MOFFS_16"
op2tag "moffs32"  = "MOFFS_32"
op2tag "moffs64"  = "MOFFS_64"
op2tag "Sreg"     = "SREG"
op2tag "FS"       = "FS"
op2tag "GS"       = "GS"
-- Below this point are operand types we have introduced
op2tag "p66"      = "PREF_66"
op2tag "pw"       = "PREF_REX_W"
op2tag "far"      = "FAR"
op2tag "label8"   = "LABEL"
op2tag "label32"  = "LABEL"
op2tag "hint"     = "HINT"
op2tag o = error $ "Unrecognized operand type: \"" ++ o ++ "\""

-- Property related
--------------------------------------------------------------------------------

-- Extract properties
properties :: Instr -> [String]
properties i = let x = map trim $ (splitOn ",") $ property i in
  filter (/= "") x

-- Implicit Read/Write/Undef related
--------------------------------------------------------------------------------

-- Expand shorthands
expand_implicit :: String -> [String]
expand_implicit "ST(*)"  = ["ST(0)","ST(1)","ST(2)","ST(3)","ST(4)","ST(5)",
                            "ST(6)","ST(7)"]
expand_implicit "TAG(*)" = ["TAG(0)","TAG(1)","TAG(2)","TAG(3)","TAG(4)",
                            "TAG(5)","TAG(6)","TAG(7)"]
expand_implicit "MM*"    = ["MM0","MM1","MM2","MM3","MM4","MM5","MM6","MM7"]
expand_implicit "XMM*"   = ["XMM0","XMM1","XMM2","XMM3","XMM4","XMM5","XMM6",
                            "XMM7","XMM8","XMM9","XMM10","XMM11","XMM12",
                            "XMM13","XMM14","XMM15"]
expand_implicit "YMM*"   = ["YMM0","YMM1","YMM2","YMM3","YMM4","YMM5","YMM6",
                            "YMM7","YMM8","YMM9","YMM10","YMM11","YMM12",
                            "YMM13","YMM14","YMM15"]
expand_implicit "E.*"    = ["E.CF","E.PF","E.AF","E.ZF","E.SF","E.SF","E.TF",
                            "E.IF","E.DF","E.OF","E.IOPL","E.NT","E.RF","E.VM",
                            "E.AC","E.VIF","E.VIP","E.ID"]
expand_implicit "S.*"    = ["S.IE","S.DE","S.ZE","S.OE","S.UE","S.PE","S.SF",
                            "S.ES","S.C0","S.C1","S.C2","S.TOP","S.C3","S.B"]
expand_implicit "M.*"    = ["M.IE","M.DE","M.ZE","M.OE","M.UE","M.PE","M.DAZ",
                            "M.IM","M.DM","M.ZM","M.OM","M.UM","M.PM","M.RC",
                            "M.FZ"]
expand_implicit "C.*"    = ["C.IM","C.DM","C.ZM","C.OM","C.UM","C.PM","C.PM",
                            "C.PC","C.RC","C.X"]
expand_implicit s = [s]

-- Extract implicit reads
implicit_reads :: Instr -> [String]
implicit_reads i = filter (/= "") $ 
                   concat $ map expand_implicit $ (splitOn " ") $ imp_read i

-- Extract implicit writes
implicit_writes :: Instr -> [String]
implicit_writes i = filter (/= "") $ 
                    concat $ map expand_implicit $ (splitOn " ") $ imp_write i

-- Extract implicit undefs
implicit_undefs :: Instr -> [String]
implicit_undefs i = filter (/= "") $ 
                    concat $ map expand_implicit $ (splitOn " ") $ imp_undef i

-- Flag related
--------------------------------------------------------------------------------

-- Separate cpuid feature flags
flags :: Instr -> [String]
flags i = filter (\x -> x /= "" ) $ splitOn " " $ flag i

-- Is this instruction VEX encoded?
is_vex_encoded :: Instr -> Bool
is_vex_encoded i = any (\x -> head x == 'V') $ opcode_terms i

--------------------------------------------------------------------------------
-- Data parsing
--------------------------------------------------------------------------------

-- Step 0: Read input file
--------------------------------------------------------------------------------

-- Read a row
read_instr :: String -> Instr
read_instr s = let (o:i:e:p:r:w:u:su:sp:m64:m32:f:a:pr:d:[]) = splitOn "\t" s in 
                   (Instr (trim o)  (trim i) 
                          e         (trim p)  --HACK: don't want to trim encoding field because we need offsets for xchg (BRC)
                          (trim r)  (trim w)  (trim u)
                          (trim su) (trim sp) (trim m64) (trim m32) 
                          (trim f) 
                          (trim a)  "" (trim d))

-- Read all rows
read_instrs :: String -> [Instr]
read_instrs s = map read_instr $ lines s

-- Step 1: Remove formatting
--------------------------------------------------------------------------------

-- Remove license, title row, and empty rows
remove_format :: [Instr] -> [Instr]
remove_format is = filter (\x -> keep x) (drop 16 is)
    where keep i = (opcode i) /= "" 

-- Step 2: Remove instructions which are invalid in 64-bit mode
--------------------------------------------------------------------------------

-- Filter out valid 64-bit application mode instructions
x64 :: [Instr] -> [Instr]
x64 is = filter keep is
  where keep i = mode64 i == "V" &&
                 useful i /= "NO" && useful i /= "NO*" &&
                 protected i /= "YES" && protected i /= "YES*"

-- Step 3: Split instructions with implicit or explicit disjunct operands
--------------------------------------------------------------------------------

-- Identifies a disjunct operand
disjunct_idx :: Instr -> Maybe Int
disjunct_idx i = findIndex disj $ operands i
  where disj o = ('/' `elem` o) || (o == "reg") || (o == "m") || (o == "mem")

-- Split a disjunct operand into its constituent parts
split_op :: String -> [String]
split_op "r/m8" = ["r8","m8"]
split_op "r/m16" = ["r16","m16"]
split_op "r/m32" = ["r32","m32"]
split_op "r/m64" = ["r64","m64"]
split_op "reg/m32" = ["r32","r64","m32"]
split_op "reg/m8" = ["r32","r64","m8"]
split_op "reg/m16" = ["r32","r64","m16"]
split_op "reg" = ["r32","r64"]
split_op "m" = ["m16","m32","m64"]
split_op "mem" = ["m16","m32","m64"]
split_op o
  | '/' `elem` o = splitOn "/" o
  | otherwise = error $ "Can't split non-disjunct operand " ++ o

-- Flatten instructions with disjunct operands
flatten_instr :: Instr -> [Instr]
flatten_instr i = case disjunct_idx i of
  Nothing    -> [i]
  (Just idx) -> map (repl_op i op) vals
    where op = (operands i) !! idx
          vals = split_op op

-- Flatten all instructions
-- Instructions can have up to two disjucnt operands (thus the double call)
flatten_instrs :: [Instr] -> [Instr]
flatten_instrs is = concat $ map flatten_instr $ 
                    concat $ map flatten_instr is

-- Step 4: Canonicalize operand symbols
--------------------------------------------------------------------------------

-- Canonical operand symbols
canonical_op :: String -> String
canonical_op "mm1"   = "mm"
canonical_op "mm2"   = "mm"
canonical_op "r32a"  = "r32"
canonical_op "r32b"  = "r32"
canonical_op "r64a"  = "r64"
canonical_op "r64b"  = "r64"
canonical_op "vm32x" = "m32"
canonical_op "vm32y" = "m32"
canonical_op "vm64x" = "m64"
canonical_op "vm64y" = "m64"
canonical_op "xmm0"  = "xmm"
canonical_op "xmm1"  = "xmm"
canonical_op "xmm2"  = "xmm"
canonical_op "xmm3"  = "xmm"
canonical_op "xmm4"  = "xmm" 
canonical_op "ymm0"  = "ymm"
canonical_op "ymm1"  = "ymm"
canonical_op "ymm2"  = "ymm"
canonical_op "ymm3"  = "ymm"
canonical_op "ymm4"  = "ymm" 
canonical_op "ST(0)" = "ST"
canonical_op "ST(i)" = "ST(i)"
canonical_op o = o

-- Canonicalize operands where synonyms were used
fix_op :: Instr -> Instr
fix_op i = i{instruction=inst}
  where inst = (raw_mnemonic i) ++ " " ++ (intercalate ", " (ops i))
        ops i = map canonical_op $ operands i

-- Canonicalize operands for all instructions
fix_ops :: [Instr] -> [Instr]
fix_ops is = map fix_op is

-- Step 5: Fix up REX rows
--------------------------------------------------------------------------------

-- Split r8 into two cases, rh and r8
fix_r8_op :: Instr -> [Instr]
fix_r8_op i = case "r8" `elem` (operands i) of
  True  -> [(repl_first_op i "r8" "rh"), (repl_first_op i "r8" "RTEMP")]
  False -> [i]

-- Change RTEMP back to r8
fix_rtemp :: Instr -> Instr
fix_rtemp i = repl_op i "RTEMP" "r8"

-- Split rows that contain r8 into two cases for each occurrance
fix_r8_row :: Instr -> [Instr]
fix_r8_row i = map fix_rtemp $ concat $ map fix_r8_op $ fix_r8_op i

-- Remove REX+ rows for instructions that don't require REX+ no matter what
remove_rex :: [Instr] -> [Instr]
remove_rex is = filter keep is
  where keep i = "REX+" `notElem` (opcode_terms i) || needs_rex i
        needs_rex i = (mn i) == "LSS" || (mn i) == "LFS" || (mn i) == "LGS"
        mn i = raw_mnemonic i

-- Remove instruction variants that both require a REX prefix, and cannot have a rex prefix
remove_invalid_rex :: [Instr] -> [Instr]
remove_invalid_rex is = filter keep is
  where keep i = not (("REX.W+" `elem` (opcode_terms i)) && ("rh" `elem` (operands i)))

-- Fix all rex rows (we do this twice to handle instructions with 2 r8 operands)
fix_rex_rows :: [Instr] -> [Instr]
fix_rex_rows is = remove_invalid_rex $ concat $ map fix_r8_row $ remove_rex is

-- Step 6: Remove duplicate rows by using the preferred encoding
--------------------------------------------------------------------------------

-- Returns same instructions with "pref" field to disambiguate
get_pref :: Int -> [Instr] -> [Instr]
get_pref index [] = []
get_pref index (i:[]) = if (index > 0)
                        then [i { pref = show index }]
                        else [i]
get_pref index is = (get_pref index [head is]) ++ (get_pref (index+1) (tail is))

-- Remove ambiguity by updaing "pref" field of otherwise identical instructions
remove_ambiguity :: [Instr] -> [Instr]
remove_ambiguity is = foldl1 (++) $ map (get_pref 0) $ groupBy eq $ sortBy srt is
  where srt x y = compare (assm_decl x) (assm_decl y)
        eq x y = (assm_decl x) == (assm_decl y)

-- Step 7: Insert prefixes and operands
--------------------------------------------------------------------------------

-- Is this an instruction which does NOT require a 66 prefix despite operands?
no_pref66 :: Instr -> Bool
no_pref66 i = (is_vex_encoded i) || 
              (op == "ENTER") ||
              (op == "FSTSW") || (op == "FNSTSW") ||
              (op == "LDDQU") ||
              (en == "LEA_R32_M16") || (en == "LEA_R32_M32") || 
              (en == "LEA_R32_M64") || (en == "LEA_R64_M16") ||
              (en == "LEA_R64_M32") || (en == "LEA_R64_M64") ||
              (en == "MOV_SREG_R16") ||
              (en == "MOVSX_R32_R16") || (en == "MOVSX_R64_R16") ||
              (en == "MOVSX_R32_M16") || (en == "MOVSX_R64_M16") ||
              (en == "MOVZX_R32_R16") || (en == "MOVZX_R64_R16") ||
              (en == "MOVZX_R32_M16") || (en == "MOVZX_R64_M16") ||
              (op == "PINSRW") ||
              (op == "RET") ||
              (op == "VERR") || (op == "VERW")
  where op = raw_mnemonic i
        en = opcode_enum i

-- Inserts PREF.66+ for instructions with 16-bit operands
-- This ignores DX which only appears as an implicit operand to string instrs
-- VEX instructions are not modifed (the override is encoded differently)
insert_pref66 :: Instr -> Instr
insert_pref66 i = case (not (no_pref66 i)) && (r16 || m16 || ax || imm16) of
  True  -> i{opcode=("PREF.66+ " ++ (opcode i))}
  False -> i
  where r16   = "r16"   `elem` (operands i)
        m16   = "m16"   `elem` (operands i) 
        ax    = "AX"    `elem` (operands i)
        imm16 = "imm16" `elem` (operands i)

-- Inserts a label variant for instructions that take rel operands
insert_label_variant :: Instr -> [Instr]
insert_label_variant i
  | "rel32" `elem` (operands i) =
    [i
    ,i{instruction=(subRegex (mkRegex "rel32") (instruction i) "label32")}]
  | "rel8" `elem` (operands i) =
    [i
    ,i {instruction=(subRegex (mkRegex "rel8") (instruction i) "label8")}]
  | otherwise = [i]

-- Inserts a hint variant for conditional jumps
insert_hint_variant :: Instr -> [Instr]
insert_hint_variant i = case is_cond_jump i of
  True -> [i,i{instruction=(instruction i ++ ", hint"),
               property=(property i ++ ", I")}]
  False -> [i]

-- Inserts everything that's missing
insert_missing :: [Instr] -> [Instr]
insert_missing is = concat $ map insert_label_variant $
                    concat $ map insert_hint_variant $
                    map insert_pref66 is

-- For each instruction, finds an equivalent instruction that doesn't use an 8-bit jump
label32_transform :: [Instr] -> String
label32_transform is = to_table is (opcode_enum . (label32_transform_find is))

-- Takes an instruction that performs an 8-bit jump and returns one that does a 32-bit jump (if possible)
label32_transform_find :: [Instr] -> Instr -> Instr
label32_transform_find is i = if "label8" `elem` (operands i) 
                              then case (find (\ins -> ((att i) == (att ins)) && 
                                                       ((head $ operands ins) == "label32")) is) of
                                Just j -> j
                                Nothing -> i
                              else i

-- Parse input file
--------------------------------------------------------------------------------

parse_instrs :: String -> IO [Instr]
parse_instrs file = do f <- readFile file
                       return $ all f
  where all = remove_ambiguity .
              insert_missing . 
              fix_rex_rows . 
              fix_ops . 
              flatten_instrs . 
              x64 . 
              remove_format . 
              read_instrs

--------------------------------------------------------------------------------
-- Debugging
--------------------------------------------------------------------------------

-- Generate a list of unique mnemonics
uniq_mnemonics :: [Instr] -> [String]
uniq_mnemonics is = nub $ map raw_mnemonic is

-- Generate a list of unique operands
uniq_operands :: [Instr] -> [String]
uniq_operands is = nub $ concat $ map nub $ map operands is 

-- Generate a list of unique operand types
uniq_operand_types :: [Instr] -> [String]
uniq_operand_types is = map op2type $ uniq_operands is

-- Generate a list of unique opcode terms
uniq_opc_terms :: [Instr] -> [String]
uniq_opc_terms is = nub $ concat $ map opcode_terms is

-- Generate a list of unique op/ens
uniq_op_en :: [Instr] -> [String]
uniq_op_en is = nub $ map op_en is

-- Generate a list of unique implict operands
uniq_implicits :: [Instr] -> [String]
uniq_implicits is = nub $ concat imps
  where imps = (map implicit_reads is) ++ (map implicit_writes is) ++ 
               (map implicit_undefs is)

-- Generate a list of ambiguous declarations
ambig_decls :: [Instr] -> [[Instr]]
ambig_decls is = filter ambig $ groupBy eq $ sortBy srt is
  where srt x y = compare (assm_decl x) (assm_decl y)
        eq x y = (assm_decl x) == (assm_decl y)
        ambig x = (length x) > 1

-- Pretty print version of ambig_decls
ambig_decls_pretty :: [Instr] -> [String]
ambig_decls_pretty is = map pretty $ ambig_decls is
  where pretty xs = (instruction (head xs)) ++ ":" ++ (concat (map elem xs))
        elem x = "\n\t" ++ (opcode x)

-- Do operand and property arities always match?
property_arity_check :: [Instr] -> IO ()
property_arity_check is = sequence_ $ map check is
  where check i = case (length (operands i)) == (length (properties i)) of
                       True -> return ()
                       False -> error $ "Property error for " ++ (opcode i)

--------------------------------------------------------------------------------
-- Codegen
-------------------------------------------------------------------------------

-- Opcode
--------------------------------------------------------------------------------

-- Converts an instruction into an Opcode enum value
opcode_enum :: Instr -> String
opcode_enum i = name ++ pref_name
  where name = intercalate "_" $ (mnem i) : (ops i)
        mnem i = raw_mnemonic i
        ops i = map (up . op2type) (operands i)
        pref_name = if (length (pref i) > 0) then ('_':(pref i)) else ""
        
-- Converts all instructions to Opcode enum values
opcode_enums :: [Instr] -> String
opcode_enums is = to_table is opcode_enum


-- Converts an instruction into an Opcode name
-- This is identical to enum values, but with at&t syntax
opcode_name :: Instr -> String
opcode_name i = low $ name ++ pref_name
  where name = intercalate "_" $ (att i) : (ops i)
        ops i = map (up . op2type) (operands i)
        pref_name = if (length (pref i) > 0) then ('_':(pref i)) else ""
 

-- Converts all instructions to names of Opcodes
opcode_names :: [Instr] -> String
opcode_names is = to_table is (\x -> low $ "\"" ++ opcode_name x ++ "\"")

-- Instruction
--------------------------------------------------------------------------------

-- Converts an instruction to arity table row
arity_row :: Instr -> String
arity_row i = show $ arity i

-- Converts all instructions to arity table
arity_table :: [Instr] -> String
arity_table is = to_table is arity_row

-- Converts an instruction to a must rex row
must_rex_row :: Instr -> String
must_rex_row i
  | "REX.W+" `elem` (opcode_prefix i) = "1"
  | "REX.R+" `elem` (opcode_prefix i) = "1"
  | "REX+"   `elem` (opcode_prefix i) = "1"
  | otherwise = "0"

-- Converts all instructions to rex table
must_rex_table :: [Instr] -> String
must_rex_table is = to_table is must_rex_row

-- Creates an entry for a property element
property_elem :: Instr -> (String, String) -> String
property_elem i (t,p) = "Properties::none()" ++ (concat (map (elem t) p))
  where elem _ 'R' = "+Property::MUST_READ"
        elem _ 'r' = "+Property::MAYBE_READ"
        elem t 'Z' = case mem_op t of 
                          True ->  "+Property::MUST_WRITE"
                          False -> "+Property::MUST_WRITE_ZX"
        elem t 'z' = case mem_op t of 
                          True ->  "+Property::MAYBE_WRITE"
                          False -> "+Property::MAYBE_WRITE_ZX"
        elem t 'W' = case (reg32_op t) of 
                          True ->  "+Property::MUST_WRITE_ZX"
                          False -> "+Property::MUST_WRITE"
        elem t 'Q' = case mem_op t of 
                          True ->  ""
                          False -> "+Property::MUST_READ"
        elem _ 'w' = "+Property::MAYBE_WRITE"
        elem _ 'U' = "+Property::MUST_UNDEF"
        elem _ 'u' = "+Property::MAYBE_UNDEF"
        elem _ 'I' = ""
        elem t c = error $ "Undefined property type " ++ t ++ ":" ++ [c]

-- Converts an instruction to properties table row
properties_row :: Instr -> String
properties_row i = "{{" ++ intercalate "," ps4 ++ "}}"
  where ps = map (property_elem i) $ zip (operands i) (properties i)
        ps4 = take 4 $ ps ++ (repeat "Properties::none()")

-- Converts all instruction to properties table
properties_table is = to_table is properties_row

-- Creates an entry for a type row
type_elem :: String -> String
type_elem o = "Type::" ++ (op2tag o)

-- Converts an instruction to type table row
type_row :: Instr -> String
type_row i = "{{" ++ intercalate "," (map type_elem (operands i)) ++ "}}"

-- Converts all instruction to type table
type_table is = to_table is type_row 

-- Converts an instruction mem_index table row
mem_index_row :: Instr -> String
mem_index_row i = case findIndex mem_or_moffs (operands i) of
  (Just idx) -> show idx
  Nothing -> "-1"
  where mem_or_moffs i = mem_op i || moffs_op i

-- Converts all instruction to mem_index table
mem_index_table :: [Instr] -> String
mem_index_table is = to_table is mem_index_row

-- Is this a must element?
is_must :: String -> Bool
is_must o = any isUpper o

-- Converts an operand to its fully qualified name
qualify_imp :: String -> String
qualify_imp s = rename s
  where rep x y s = subRegex (mkRegex x) s y
        rename s = rep "FPUDATA" "fpu_data" $
                   rep "FPUINSTR" "fpu_instruction" $
                   rep "FPUOPCODE" "fpu_opcode" $
                   rep "RIP" "rip" $
                   rep "ST\\((.)\\)" "ST\\1" $
                   rep "st\\((.)\\)" "st\\1" $
                   rep "TAG\\((.)\\)" "TAG\\1" $
                   rep "tag\\((.)\\)" "tag\\1" $
                   rep "E\\." "eflags_" $
                   rep "e\\." "eflags_" $
                   rep "C\\." "fpu_control_" $
                   rep "c\\." "fpu_control_" $ 
                   rep "S\\." "fpu_status_" $
                   rep "s\\." "fpu_status_" $
                   rep "M\\." "mxcsr_" $
                   rep "m\\." "mxcsr_" $ s

-- Converts an instruction to implicit_read table row
must_read_row :: Instr -> String
must_read_row i 
  | "???" `elem` (implicit_reads i) = "RegSet::universe()"
  | otherwise = "RegSet::empty()" ++ (concat (map val (irs i)))
    where irs i = filter is_must $ map qualify_imp $ implicit_reads i
          val s = "+Constants::" ++ (low s) ++ "()"

-- Converts all instructions to implicit_read table
must_read_table :: [Instr] -> String
must_read_table is = to_table is must_read_row 

-- Converts an instruction to implicit_read table row
maybe_read_row :: Instr -> String
maybe_read_row i 
  | "???" `elem` (implicit_reads i) = "RegSet::universe()"
  | otherwise = "RegSet::empty()" ++ (concat (map val (irs i)))
    where irs i = map qualify_imp $ implicit_reads i
          val s = "+Constants::" ++ (low s) ++ "()"

-- Converts all instructions to implicit_read table
maybe_read_table :: [Instr] -> String
maybe_read_table is = to_table is maybe_read_row 

-- Converts an instruction to implicit_write table row
must_write_row :: Instr -> String
must_write_row i 
  | "???" `elem` (implicit_writes i) = "RegSet::universe()"
  | otherwise = "RegSet::empty()" ++ (concat (map val (iws i)))
    where iws i = filter is_must $ map qualify_imp $ implicit_writes i
          val s = "+Constants::" ++ (low s) ++ "()"

-- Converts all instructions to implicit_write table
must_write_table :: [Instr] -> String
must_write_table is = to_table is must_write_row

-- Converts an instruction to implicit_write table row
maybe_write_row :: Instr -> String
maybe_write_row i 
  | "???" `elem` (implicit_writes i) = "RegSet::universe()"
  | otherwise = "RegSet::empty()" ++ (concat (map val (iws i)))
    where iws i = map qualify_imp $ implicit_writes i
          val s = "+Constants::" ++ (low s) ++ "()"

-- Converts all instructions to implicit_write table
maybe_write_table :: [Instr] -> String
maybe_write_table is = to_table is maybe_write_row

-- Converts an instruction to implicit_undef table row
must_undef_row :: Instr -> String
must_undef_row i 
  | "???" `elem` (implicit_undefs i) = "RegSet::universe()"
  | otherwise = "RegSet::empty()" ++ (concat (map val (ius i)))
    where ius i = filter is_must $ map qualify_imp $ implicit_undefs i
          val s = "+Constants::" ++ (low s) ++ "()"

-- Converts all instructions to implicit_undef table
must_undef_table :: [Instr] -> String
must_undef_table is = to_table is must_undef_row

-- Converts an instruction to implicit_undef table row
maybe_undef_row :: Instr -> String
maybe_undef_row i 
  | "???" `elem` (implicit_undefs i) = "RegSet::universe()"
  | otherwise = "RegSet::empty()" ++ (concat (map val (ius i)))
    where ius i = map qualify_imp $ implicit_undefs i
          val s = "+Constants::" ++ (low s) ++ "()"

-- Converts all instructions to implicit_undef table
maybe_undef_table :: [Instr] -> String
maybe_undef_table is = to_table is maybe_undef_row

-- Converts an instruction to a flag table row
flag_row :: Instr -> String
flag_row i = "FlagSet::empty()" ++ (concat (map elem (flags i)))
  where elem x = "+Flag::" ++ x

-- Converts all instructions to flag table
flag_table :: [Instr] -> String
flag_table is = to_table is flag_row

-- Converts an instruction to a printable at&t mnemonic
att_mnemonic :: Instr -> String
att_mnemonic i = "\"" ++ (att i) ++ "\""

-- Converts all instructions to printable at&t mnemonics
att_mnemonics :: [Instr] -> String
att_mnemonics is = intercalate "\n" $ map (", "++) $ map att_mnemonic is

-- Converts an instruction to a printable Intel mnemonic
intel_mnemonic :: Instr -> String
intel_mnemonic i = "\"" ++ (low (raw_mnemonic i)) ++ "\""

-- Converts all instructions to printable Intel mnemonics
intel_mnemonics :: [Instr] -> String
intel_mnemonics is = intercalate "\n" $ map (", "++) $ map intel_mnemonic is

-- Common Assembler strings
--------------------------------------------------------------------------------

-- Assembler mnemonic
assm_mnemonic :: Instr -> String
assm_mnemonic i = let m = raw_mnemonic i in
  case m of
    "AND" -> "and_"
    "INT" -> "int_"
    "NOT" -> "not_"
    "OR"  -> "or_"
    "STD" -> "std_"
    "XOR" -> "xor_"
    _     -> (low m)

-- Assembler doxygen comment
assm_doxy :: Instr -> String
assm_doxy i = "/** " ++ (description i) ++ " */"

-- Assembler arg type
assm_arg_type :: String -> String
assm_arg_type a = "const " ++ (op2type a) ++ "&"

-- Assembler declaration arg list
assm_arg_list :: Instr -> String
assm_arg_list i = intercalate ", " $ map arg $ zip [0..] (operands i)
  where arg (i,a) = (assm_arg_type a) ++ " arg" ++ (show i)

-- Assembler declaration
assm_decl :: Instr -> String
assm_decl i = "void " ++
              (assm_mnemonic i) ++
              (if (length (pref i) > 0) then '_':(pref i) else "") ++
              "(" ++
              (assm_arg_list i) ++
              ")"

-- Assembler header declarations
--------------------------------------------------------------------------------

-- Assembler header declaration
assm_header_decl :: Instr -> String
assm_header_decl i = (assm_doxy i) ++ "\n" ++ (assm_decl i) ++ ";"

-- Assembler header declarations
assm_header_decls :: [Instr] -> String
assm_header_decls is = intercalate "\n\n" $ map assm_header_decl is

-- Assembler source definitions
--------------------------------------------------------------------------------

-- Emits code for the FWAIT prefix
pref_fwait :: Instr -> String
pref_fwait i
  | "9B" `elem` opcode_prefix i = "pref_fwait(0x9b);\n"
  | otherwise = "// No FWAIT Prefix\n"

-- Emits code for Prefix Group 1
-- This doesn't check for the the lock prefix which we treat as an opcode
pref1 :: Instr -> String
pref1 i 
  | "F2" `elem` opcode_prefix i = "pref_group1(0xf2);\n"
  | "F3" `elem` opcode_prefix i = "pref_group1(0xf3);\n"
  | otherwise = "// No Prefix Group 1\n"

-- Emits code for Prefix Group 2
pref2 :: Instr -> String
pref2 i
  | "hint" `elem` operands i = "pref_group2(arg1);\n"
  | otherwise = case findIndex mem_op (operands i) of
                     (Just idx) -> "pref_group2(arg" ++ (show idx) ++ ");\n"
                     Nothing -> "// No Prefix Group 2\n"

-- Emits code for Prefix Group 3 (operand size override)
pref3 :: Instr -> String
pref3 i 
  | "PREF.66+" `elem` opcode_prefix i = "pref_group3();\n"
  | "66" `elem` opcode_prefix i = "pref_group3();\n"
  | otherwise = "// No Prefix Group 3\n"

-- Emits code for Prefix Group 4 (address size override)
pref4 :: Instr -> String
pref4 i = case findIndex mem_op (operands i) of
               (Just idx) -> "pref_group4(arg" ++ (show idx) ++ ");\n"
               Nothing -> "// No Prefix Group 4\n"

-- Explicit MOD/RM and REX args
rm_args :: Instr -> String
rm_args i = case op_en i of
  "MI"   -> "arg0"
  "MR"   -> "arg0,arg1"
  "RM"   -> "arg1,arg0"
  "RMI"  -> "arg1,arg0"
  "RM0"  -> "arg1,arg0"
  "M"    -> "arg0"
  "MRI"  -> "arg0,arg1"
  "RVM"  -> "arg2,arg0"
  "RMV"  -> "arg1,arg0"
  "MC"   -> "arg0"
  "M1"   -> "arg0"
  "MRC"  -> "arg0,arg1"
  "RMVI" -> "arg1,arg0"
  "RVMI" -> "arg2,arg0"
  "RVMR" -> "arg2,arg0"
  "MVR"  -> "arg0,arg2"
  "XM"   -> "arg1,arg0"
  "VM"   -> "arg1"
  "VMI"  -> "arg1"
  _      -> ""

-- Optional Mod R/M SIB digit argument
digit :: Instr -> String
digit i = case find is_digit (opcode_suffix i) of
  (Just ('/':d:[])) -> ",Constants::r64s()[" ++ [d] ++ "]"
  Nothing -> ""

-- Implied rex values
implied_rex :: Instr -> String
implied_rex i
  | "REX.W+" `elem` (opcode_prefix i) = "rex_w()"
  | "REX.R+" `elem` (opcode_prefix i) = "rex_r()"
  | "REX+"   `elem` (opcode_prefix i) = "rex()"
  | otherwise = "(uint8_t)0x00"

-- Emits code for REX Prefix 
rex_prefix :: Instr -> String
rex_prefix i 
  | op_en i == "O" = "rex(arg0," ++ (implied_rex i) ++ ");\n"
  | op_en i == "OI" = "rex(arg0," ++ (implied_rex i) ++ ");\n"
  | op_en i == " O" = "rex(arg1," ++ (implied_rex i) ++ ");\n"
  | rm_args i /= "" = "rex(" ++ (rm_args i) ++ "," ++ (implied_rex i) ++ ");\n"
  | implied_rex i /= "(uint8_t)0x00" = "rex(" ++ (implied_rex i) ++ ");\n"
  | otherwise = "// No REX Prefix\n"

-- Explicit VEX mmmmm arg
vex_mmmmm :: Instr -> String
vex_mmmmm i
  | "0F"   `elem` (vex_terms i) = "0x01"
  | "0F38" `elem` (vex_terms i) = "0x02"
  | "0F3A" `elem` (vex_terms i) = "0x03"
  | otherwise = "0x01"

-- Explicit VEX l arg
vex_l :: Instr -> String
vex_l i 
  | "256" `elem` (vex_terms i) = "0x1"
  | otherwise = "0x0"

-- Explicit VEX pp arg
vex_pp :: Instr -> String
vex_pp i 
  | "66" `elem` (vex_terms i) = "0x1"
  | "F3" `elem` (vex_terms i) = "0x2"
  | "F2" `elem` (vex_terms i) = "0x3"
  | otherwise = "0x0"

-- Default VEX w value
vex_w :: Instr -> String
vex_w i 
  | "W1" `elem` (vex_terms i) = "0x1"
  | otherwise = "0x0"

-- Explicit VEX vvvv arg
vex_vvvv :: Instr -> String
vex_vvvv i = case findIndex (=='V') (op_en i) of
  (Just idx) -> "arg" ++ (show idx) 
  Nothing -> "Constants::xmm0()"

-- Emits code for VEX Prefix
vex_prefix :: Instr -> String
vex_prefix i = "vex(" ++ 
               (vex_mmmmm i) ++ "," ++
               (vex_l i) ++ "," ++
               (vex_pp i) ++ "," ++
               (vex_w i) ++ "," ++
               (vex_vvvv i) ++ 
               (case (rm_args i) of
                  "" -> ""
                  _  -> "," ++ (rm_args i) ++ (digit i)) ++ 
               ");\n"

-- Emits code for VEX opcodes
vex_opcode :: Instr -> String
vex_opcode i = "opcode(0x" ++ (low ((opcode_terms i)!!1)) ++ ");\n"

-- Emits code for non-VEX encoded opcode bytes
non_vex_opcode :: Instr -> String
non_vex_opcode i 
  | (opcode_bytes i) == [] = "// No Opcode Bytes"
  | otherwise = "opcode(" ++ (bytes i) ++ (code i) ++ ");\n" 
    where bytes i = intercalate "," $ map (("0x"++).low) (opcode_bytes i)
          code i = case findIndex (=='O') (op_en i) of
                        (Just idx) -> ",arg" ++ (show idx)
                        Nothing -> ""

-- Emits code for mod/rm and sib bytes
mod_rm_sib :: Instr -> String
mod_rm_sib i = case rm_args i of
    "" -> "// No MOD R/M or SIB Bytes\n"
    _ -> "mod_rm_sib(" ++ (rm_args i) ++ (digit i) ++ ");\n"

-- Does this instruction have a displacement or immediate operand?
disp_imm_index :: Instr -> Maybe Int
disp_imm_index i = findIndex disp_imm_op (operands i)
  where disp_imm_op o = imm_op o || moffs_op o || rel_op o

-- Emits code for displacement or immediate bytes
disp_imm :: Instr -> String
disp_imm i 
  | op_en i == "II" = "disp_imm(arg0,arg1);\n"
  | otherwise = case disp_imm_index i of
                     (Just idx) -> "disp_imm(arg" ++ (show idx) ++ ");\n"
                     Nothing -> "// No Displacement/Immediate\n"

-- Emits code for label displacement
disp_label :: Instr -> String
disp_label i = case (findIndex label_op (operands i)) of
              (Just idx) -> case (operands i) !! idx of
                "label8" -> "disp_label8(arg" ++ (show idx) ++ ");\n"
                "label32" -> "disp_label32(arg" ++ (show idx) ++ ");\n"
              Nothing -> "// No label displacement\n"

-- Emits code for vex immediate byte
vex_imm :: Instr -> String
vex_imm i = case "/is4" `elem` (opcode_suffix i) of
  True -> "disp_imm(arg3);\n"
  False -> "// No VEX Immediate\n"

-- Emits pre-assembly debug statement
assm_debug_begin :: Instr -> String
assm_debug_begin i = "\t#ifdef DEBUG_ASSEMBLER\n" ++
                     "\t\tsize_t debug_i = fxn_->size();\n" ++
                     "\t#endif\n\n"

-- Emits post-assembly debug statement
assm_debug_end :: Instr -> String
assm_debug_end i = "\t#ifdef DEBUG_ASSEMBLER\n" ++
                   "\t\tdebug(" ++ instr ++ ", debug_i);\n" ++
                   "\t#endif\n"
  where instr = "Instruction{" ++ (opc i) ++ ",{" ++ (ops i) ++ "}}"
        opc i = opcode_enum i 
        ops i = intercalate "," $ map (("arg"++).show) $ take (arity i) [0..]

-- VEX encoded instruction definition
assm_vex_defn :: Instr -> String
assm_vex_defn i = "  // VEX-Encoded Instruction: \n\n" ++
                  "  // Prefix Group 1 is #UD for VEX\n" ++
                  "  " ++ pref2 i ++ 
                  "  // Prefix Group 3 is #UD for VEX\n" ++
                  "  " ++ pref4 i ++
                  "  " ++ vex_prefix i ++ 
                  "  " ++ vex_opcode i ++
                  "  " ++ mod_rm_sib i ++
                  "  " ++ disp_imm i ++
                  "  " ++ disp_label i ++
                  "  " ++ vex_imm i ++
                  "  \n"

-- Other instruction definition
assm_oth_defn :: Instr -> String
assm_oth_defn i = "  // Non-VEX-Encoded Instruction: \n\n" ++
                  "  " ++ pref_fwait i ++ 
                  "  " ++ pref2 i ++ 
                  "  " ++ pref4 i ++ -- gcc prefers this ordering
                  "  " ++ pref3 i ++ -- gcc prefers this ordering
                  "  " ++ pref1 i ++ -- gcc prefers this ordering
                  "  " ++ rex_prefix i ++
                  "  " ++ non_vex_opcode i ++
                  "  " ++ mod_rm_sib i ++
                  "  " ++ disp_imm i ++
                  "  " ++ disp_label i ++
                  "  \n"

-- Assembler src definition
assm_src_defn :: Instr -> String
assm_src_defn i = "void Assembler::" ++
                  (assm_mnemonic i) ++
                  (if (length (pref i) > 0) then '_':(pref i) else "") ++
                  "(" ++
                  (assm_arg_list i) ++
                  ") {\n" ++
                  assm_debug_begin i ++
                  body i ++ 
                  assm_debug_end i ++ 
                  "}"
  where body i = case is_vex_encoded i of
                      True  -> assm_vex_defn i
                      False -> assm_oth_defn i

-- Assembler src definitions
assm_src_defns :: [Instr] -> String
assm_src_defns is = intercalate "\n\n" $ map assm_src_defn is

-- Assembler switch code
--------------------------------------------------------------------------------

-- Assembler switch args
assm_call_arg_list :: Instr -> String
assm_call_arg_list i = intercalate ", " $ map arg $ zip [0..] (operands i)
  where arg (i,a) = "instr.get_operand<" ++ (op2type a) ++ ">(" ++ (show i) ++ ")"

-- Assembler switch call
assm_call :: Instr -> String
assm_call i = (assm_mnemonic i) ++ 
              (if (length (pref i) > 0) then '_':(pref i) else "") ++
              "(" ++ (assm_call_arg_list i) ++ ");"

-- Assembler switch case
assm_case :: Instr -> String
assm_case i = "case " ++ (opcode_enum i) ++ ":\n" ++
              "\t" ++ (assm_call i) ++ "\n" ++
              "\tbreak;"

-- All assembler switch cases
assm_cases :: [Instr] -> String
assm_cases is = intercalate "\n" $ map assm_case is

-- Instruction ordering
--------------------------------------------------------------------------------

-- Comparison ordering for operands (more specific appear first)
op_order :: [String]
op_order = ["hint",
  "0","1","3","imm8","imm16","imm32","imm64",
  "label32", "label8",
  "p66","pw","far",
  "rh","AL","CL","r8","AX","DX","r16","EAX","r32","RAX","r64",
  "rel8","rel32",
  "moffs8","moffs16","moffs32","moffs64",
  "m8","m16","m32","m64","m128","m256","m16:16","m16:32","m16:64",
  "m16int","m32int","m64int","m80bcd","m32fp","m64fp","m80fp",
  "m2byte","m28byte","m108byte","m512byte",
  "mm",
  "FS","GS","Sreg",
  "ST","ST(i)",
  "<XMM0>","xmm",
  "ymm"]

-- Compare two operands
compare_op :: String -> String -> Ordering
compare_op o1 o2 = compare (idx o1 op_order) (idx o2 op_order)
  where idx x xs = let (Just i) = elemIndex x xs in i

-- Compare operand lists
compare_ops :: [String] -> [String] -> Ordering
compare_ops [] [] = EQ
compare_ops [] _ = LT
compare_ops _ [] = GT
compare_ops (x:xs) (y:ys) = case compare_op x y of
  LT -> LT
  GT -> GT
  EQ -> compare_ops xs ys

-- Compare instructions based on operands, defer to preference info
compare_pref :: Instr -> Instr -> Ordering
compare_pref i1 i2
  | (pref i1 == "YES") && (pref i2 == "YES") = EQ
  | (pref i1 == "YES") = LT
  | (pref i2 == "YES") = GT
  | otherwise = EQ

-- Compare instructions based on operands, defer to preference info
compare_instr :: Instr -> Instr -> Ordering
compare_instr i1 i2 = case compare_ops (reverse $ operands i1) (reverse $ operands i2) of
  LT -> LT
  GT -> GT
  EQ -> compare_pref i1 i2

-- Read AT&T code
--------------------------------------------------------------------------------

-- Sort instructions by at&t mnemonic
att_sort :: [Instr] -> [Instr]
att_sort is = sortBy (\x y -> compare (att x) (att y)) is

-- Group instructions by at&t mnemonic
att_group :: [Instr] -> [[Instr]]
att_group is = groupBy (\x y -> (att x) == (att y)) is'
  where is' = att_sort is

-- Generates a part of a row in the at&t parse table
att_row_elem :: Instr -> String
att_row_elem i = "{" ++ e ++ ", vector<Type>{" ++ ops ++ "}}"
  where e = opcode_enum i
        ops = intercalate "," $ map (("Type::"++).op2tag) $ operands i

-- Generates a row in the at&t parse table
att_row :: String -> [Instr] -> String
att_row suffix is = " \t{\"" ++ (mn is) ++ suffix ++ "\", {{" ++ target ++ ",{" ++ imm_sizes_text ++ "}}, {\n\t\t " ++ (body is) ++ "\n}}}"
  where mn is = (att (head is))
        body is = intercalate "\n\t\t," $ map att_row_elem $ sortBy compare_instr is
        all_operands = concat $ map operands is
        all_sizes = nub $ filter (\x -> x /= 0) $ map op_width $ all_operands
        imm_sizes = nub $ filter (\x -> x /= 0) $ map op_width $ filter imm_op $ all_operands
        imm_sizes_text = intercalate "," $ map show imm_sizes
        target = show $ case (length all_sizes) of
                          0 -> 0
                          _ -> maximum all_sizes

-- Generates the entire at&t parse table
att_table :: [Instr] -> String
att_table is = intercalate "\n, " (regular_rows ++ alternate_rows)
  where regular_rows = map (att_row "") $ att_group is
        alternate_rows = map (att_row ".s") $ att_group $ alternates is
        alternates is = filter (\i -> pref i /= "") is

-- Write code
--------------------------------------------------------------------------------

write_code :: [Instr] -> IO ()
write_code is = do writeFile "assembler.decl"         $ assm_header_decls is
                   writeFile "assembler.defn"         $ assm_src_defns is
                   writeFile "assembler.switch"       $ assm_cases is
                   writeFile "arity.table"            $ arity_table is
                   writeFile "rex.table"              $ must_rex_table is
                   writeFile "properties.table"       $ properties_table is
                   writeFile "type.table"             $ type_table is
                   writeFile "mem_index.table"        $ mem_index_table is
                   writeFile "must_read.table"        $ must_read_table is
                   writeFile "maybe_read.table"       $ maybe_read_table is
                   writeFile "must_write.table"       $ must_write_table is
                   writeFile "maybe_write.table"      $ maybe_write_table is
                   writeFile "must_undef.table"       $ must_undef_table is
                   writeFile "maybe_undef.table"      $ maybe_undef_table is
                   writeFile "flag.table"             $ flag_table is
                   writeFile "opcode.enum"            $ opcode_enums is
                   writeFile "opcode.att"             $ att_mnemonics is
                   writeFile "opcode.intel"           $ intel_mnemonics is
                   writeFile "att.table"              $ att_table is
                   writeFile "opcode.names"           $ opcode_names is
                   writeFile "opcode.l32_transform"   $ label32_transform is

--------------------------------------------------------------------------------
-- Main (read the spreadsheet and write some code)
--------------------------------------------------------------------------------

main :: IO ()
main = do is <- parse_instrs "x86.csv"       
          property_arity_check is 
          write_code is
