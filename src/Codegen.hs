import Data.Char
import Data.List
import Data.List.Split
import System.Environment
import Text.Regex
import Text.Regex.TDFA

-------------------------------------------------------------------------------
-- Data Types 
-------------------------------------------------------------------------------

-- Instruction Row Type
data Instr =
  Instr { opcode      :: String
        , instruction :: String
        , op_en       :: String
        , property    :: String
        , mode64      :: String
        , mode32      :: String				
        , flag        :: String
        , att         :: String
        , description :: String
        } deriving (Show)

-------------------------------------------------------------------------------
-- Helper Methods
-------------------------------------------------------------------------------

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

-- Transforms a list of instructions into a comma separated table
to_table :: [Instr] -> (Instr -> String) -> String
to_table is f = intercalate "\n" $ map elem is
  where elem i = ", " ++ (f i) ++ " // " ++ instruction i

-------------------------------------------------------------------------------
-- Read Input File
-------------------------------------------------------------------------------

-- Step 0: Read input file
-------------------------------------------------------------------------------

-- Read a row
read_instr :: String -> Instr
read_instr s = let (o:i:e:p:m64:m32:f:a:d:[]) = splitOn "\t" s in 
                   (Instr (trim o) (trim i) 
                          (trim e) (trim p)
                          (trim m64) (trim m32) 
                          (trim f) 
                          (trim a) (trim d))

-- Read all rows
read_instrs :: String -> [Instr]
read_instrs s = map read_instr $ lines s

-- Step 1: Remove formatting
-------------------------------------------------------------------------------

-- Remove title row and empty rows		
remove_format :: [Instr] -> [Instr]
remove_format is = filter (\x -> keep x) is
    where keep i = (opcode i) /= "" && 
                   (opcode i) /= "Opcode" &&
                   (instruction i) /= "(No mnemonic)"

-- Step 2: Remove instructions which are invalid in 64-bit mode
-------------------------------------------------------------------------------

-- Filters out valid 64-bit mode instructions
x64 :: [Instr] -> [Instr]
x64 is = filter (\x -> (mode64 x) == "V") is

-- Step 3: Split instructions with implicit or explicit disjunct operands
-------------------------------------------------------------------------------

-- Identifies a disjunct operand
disjunct_idx :: Instr -> Maybe Int
disjunct_idx i = findIndex d $ operands i
  where d o = ('/' `elem` o) || (o == "reg") || (o == "m") || (o == "mem")

-- Split a disjunct operand into parts
split_op :: String -> [String]
split_op "r/m8" = ["r8","m8"]
split_op "r/m16" = ["r16","m16"]
split_op "r/m32" = ["r32","m32"]
split_op "r/m64" = ["r64","m64"]
split_op "reg/m32" = ["r32","r64","m32"]
split_op "m14/28byte" = ["m14byte","m28byte"]
split_op "m94/108byte" = ["m94byte","m108byte"]
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
-------------------------------------------------------------------------------

-- Canonical operand symbols
canonical_op :: String -> String
canonical_op "mm1"   = "mm"
canonical_op "mm2"   = "mm"
canonical_op "xmm1"  = "xmm"
canonical_op "xmm2"  = "xmm"
canonical_op "xmm3"  = "xmm"
canonical_op "xmm4"  = "xmm"
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
-------------------------------------------------------------------------------

-- Split a row with an r8 operand into two alternatives
split_r8 :: String -> String -> Instr -> [Instr]
split_r8 alt1 alt2 i = case "r8" `elem` (operands i) of
  True ->  [(repl_first_op i "r8" alt1), (repl_first_op i "r8" alt2)]
  False -> [i]

-- Replace r8 in rew rows by rl and rb
fix_rex_r8 :: Instr -> [Instr]
fix_rex_r8 i = case "REX+" `elem` (opcode_terms i) of
  True  -> concat $ map (split_r8 "rl" "rb") $ split_r8 "rl" "rb" i
  False -> [i]

-- Replace r8 in non-rex rows by rl and rh
fix_norex_r8 :: Instr -> [Instr]
fix_norex_r8 i = case "REX+" `notElem` (opcode_terms i) of
  True  -> concat $ map (split_r8 "rl" "rh") $ split_r8 "rl" "rh" i
  False -> [i]

-- Replace an r8 in a rex row if necessary
fix_rex_row :: Instr -> [Instr]
fix_rex_row i = concat $ map fix_norex_r8 $ fix_rex_r8 i

-- Does this row have r8 operands?
r8_row :: Instr -> Bool
r8_row i = "rl" `elem` os || "rh" `elem` os || "rb" `elem` os
  where os = operands i

-- Is this one of three instructions that require REX+ no matter what
needs_rex :: Instr -> Bool
needs_rex i = mn == "LSS" || mn == "LFS" || mn == "LGS"
  where mn = raw_mnemonic i

-- Remove REX+ rows which correspond to r/m8 splits
remove_m8_rex :: [Instr] -> [Instr]
remove_m8_rex is = filter keep is
  where keep i = "REX+" `notElem` (opcode_terms i) || r8_row i || needs_rex i

-- Fix all rex rows
fix_rex_rows :: [Instr] -> [Instr]
fix_rex_rows is = remove_m8_rex $ concat $ map fix_rex_row is

-- Step 6: Remove duplicate rows by prefering shortest encoding
-------------------------------------------------------------------------------

-- Returns the instruction with the shortest encoding
shortest :: [Instr] -> Instr
shortest is = minimumBy encoding is
  where encoding i1 i2 = compare (len i1) (len i2)
        len i = (length (opcode_terms i))

-- Remove ambiguity by prefering the shortest encoding
remove_ambiguity :: [Instr] -> [Instr]
remove_ambiguity is = map shortest $ groupBy eq $ sortBy srt is
  where srt x y = compare (assm_decl x) (assm_decl y)
        eq x y = (assm_decl x) == (assm_decl y)	

-- Step 7: Insert prefixes and operands
-------------------------------------------------------------------------------

-- Inserts PREF.66+ for instructions with 16-bit operands
-- This ignores DX which I think is always an implicit operand (CHECK THIS)
-- NOTE: This is unnecessary for the p66 operand which comes with PREF.66+
insert_pref66 :: Instr -> Instr
insert_pref66 i = case r16 || m16 || ax || imm16 of
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
    ,i{instruction=(subRegex (mkRegex "rel32") (instruction i) "label")
      ,opcode=(subRegex (mkRegex "cd") (opcode i) "0d")}]	
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

-------------------------------------------------------------------------------
-- Debugging
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Views (transformations on row data into usable forms)
-------------------------------------------------------------------------------

-- Separate opcode terms
opcode_terms :: Instr -> [String]
opcode_terms i = splitOn " " (opcode i)

-- Is this opcode term a prefix?
-- Does it refer to bytes which must appear before the rex prefix?
-- 66 is a mandatory prefix with different semantics from PREF.66+.
-- Ditto for F2 and F3 which are otherwise REP prefixes.
is_prefix :: String -> Bool
is_prefix "PREF.66+" = True
is_prefix "REX.W+" = True
is_prefix "REX.R+" = True
is_prefix "REX+" = True
is_prefix "66" = True
is_prefix "F2" = True
is_prefix "F3" = True
is_prefix _ = False

-- Is this opcode term a suffix?
is_suffix :: String -> Bool
is_suffix ('+':_) = True
is_suffix ('/':_) = True
is_suffix ('i':_) = True
is_suffix ('c':_) = True
is_suffix _ = False

-- Is this opcode term a digit?
is_digit :: String -> Bool
is_digit ('/':d:_) = isDigit d
is_digit _ = False

-- Extracts opcode prefix bytes
opcode_prefix :: Instr -> [String]
opcode_prefix i = takeWhile is_prefix $ opcode_terms i

-- Extracts opcode bytes (non rex prefix or suffix terms) from opcode terms
opcode_bytes :: Instr -> [String]
opcode_bytes i = (dropWhile is_prefix) . (takeWhile not_suffix) $ ts
  where not_suffix t = not $ is_suffix t
        ts = opcode_terms i

-- Extracts opcode suffix bytes
opcode_suffix :: Instr -> [String]
opcode_suffix i = (dropWhile is_prefix) . (dropWhile not_suffix) $ ts
  where not_suffix t = not $ is_suffix t
        ts = opcode_terms i

-- Is this a register coded opcode instruction?
is_register_coded :: Instr -> Bool
is_register_coded i
  | "+RB" `elem` opcode_suffix i = True
  | "+RW" `elem` opcode_suffix i = True
  | "+RD" `elem` opcode_suffix i = True
  | "+RO" `elem` opcode_suffix i = True
  | "+I"  `elem` opcode_suffix i = True
  | otherwise = False

-- Returns true for an operand which is a register code parameter
is_register_code_arg :: String -> Bool
is_register_code_arg "rl" = True
is_register_code_arg "rh" = True
is_register_code_arg "rb" = True
is_register_code_arg "r16" = True
is_register_code_arg "r32" = True
is_register_code_arg "r64" = True
is_register_code_arg "ST(i)" = True
is_register_code_arg _ = False

-- Extracts raw mnemonic from instruction
raw_mnemonic :: Instr -> String
raw_mnemonic i = head $ words $ instruction i

-- Extract operands from instruction
operands :: Instr -> [String]
operands i = let x = (splitOn ",") $ concat $ tail $ words (instruction i) in
	filter (\o -> o /= "") x

-- Extract properties from property
properties :: Instr -> [String]
properties i = let x = map trim $ (splitOn ",") $ property i in
  filter (\p -> p /= "") x

-- Transform operand notation into type
op2type :: String -> String
op2type "rl"       = "Rl"
op2type "rh"       = "Rh"
op2type "rb"       = "Rb"
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
op2type "m16&64"   = "MPair1664"
op2type "m16:16"   = "MPtr1616" 
op2type "m16:32"   = "MPtr1632"
op2type "m16:64"   = "MPtr1664"
op2type "m16int"   = "M16Int"
op2type "m32int"   = "M32Int"
op2type "m64int"   = "M64Int"
op2type "m80bcd"   = "M80Bcd" 
op2type "m32fp"    = "M32Fp"
op2type "m64fp"    = "M64Fp"
op2type "m80fp"    = "M80Fp"
op2type "m2byte"   = "M2Byte" 
op2type "m14byte"  = "M14Byte" 
op2type "m28byte"  = "M28Byte" 
op2type "m94byte"  = "M94Byte" 
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
op2type "CR0-CR7"  = "Cr0234" 
op2type "CR8"      = "Cr8"
op2type "DR0-DR7"  = "Dr" 
op2type "Sreg"     = "Sreg"
op2type "FS"       = "Fs"
op2type "GS"       = "Gs"
-- Below this point are operand types we have introduced
op2type "p66"      = "Pref66"
op2type "pw"       = "PrefRexW"
op2type "far"      = "Far"
op2type "norexr8"  = "NoRexR8"
op2type "label"    = "Label"
op2type "hint"     = "Hint"
op2type o = error $ "Unrecognized operand type: \"" ++ o ++ "\""

-- Separate cpuid feature flags
flags :: Instr -> [String]
flags i = splitOn " " $ flag i

-- Is this instruction VEX encoded?
is_vex_encoded :: Instr -> Bool
is_vex_encoded i = ("AVX" `elem` fs) || ("F16C" `elem` fs)
  where fs = flags i

-- Is this a string instruction?
is_string_instr :: Instr -> Bool
is_string_instr i = let mn = raw_mnemonic i in
  (mn == "CMPS")  || 
	(mn == "CMPSB") || (mn == "CMPSW") || (mn == "CMPSD") || (mn =="CMPSQ") ||
  (mn == "INS")   || 
	(mn == "INSB")  || (mn == "INSW")  || (mn == "INSD")  ||
  (mn == "LODS")  || 
	(mn == "LODSB") || (mn == "LODSW") || (mn == "LODSD") || (mn =="LODSQ") ||
  (mn == "MOVS")  || 
	(mn == "MOVSB") || (mn == "MOVSW") || (mn == "MOVSD") || (mn =="MOVSQ") ||
  (mn == "OUTS")  ||
	(mn == "OUTB")  || (mn == "OUTSW") || (mn == "OUTSD") ||
  (mn == "SCAS")  || 
	(mn == "SCASB") || (mn == "SCASW") || (mn == "SCASD") || (mn =="SCASQ") ||
  (mn == "STOS")  || 
	(mn == "STOSB") || (mn == "STOSW") || (mn == "STOSD") || (mn =="STOSQ")

-- Is this a conditional jump?
is_cond_jump :: Instr -> Bool
is_cond_jump i = let mn = raw_mnemonic i in
  head mn == 'J' && mn /= "JMP"

-- Is this an unconditional jump?
is_uncond_jump :: Instr -> Bool
is_uncond_jump i = raw_mnemonic i == "JMP"

-- Returns true for register operands
reg_op :: String -> Bool
reg_op "rl"  = True
reg_op "rh"  = True
reg_op "rb"  = True
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
mem_op "m16&64"   = True
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
mem_op "m14byte"  = True
mem_op "m28byte"  = True
mem_op "m94byte"  = True
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

-- Returns true for label operands
label_op :: String -> Bool
label_op "label" = True
label_op _ = False

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

-- Returns true for cr operands
cr_op :: String -> Bool
cr_op "CR0-CR7" = True
cr_op "CR8"     = True
cr_op _         = False

-- Returns true for xmm operands
xmm_op :: String -> Bool
xmm_op "xmm"    = True
xmm_op "<XMM0>" = True
xmm_op _        = False

-- Returns true for any form of displacement or immediate operand
disp_imm_op :: String -> Bool
disp_imm_op o = imm_op o || moffs_op o || rel_op o || label_op o

-- Does this instruction have a memory operand?
mem_index :: Instr -> Maybe Int
mem_index i = findIndex mem_op (operands i) 

-- Does this instruction have a displacement or immediate operand
disp_imm_index :: Instr -> Maybe Int
disp_imm_index i = findIndex disp_imm_op (operands i)

-------------------------------------------------------------------------------
-- code codegen
-------------------------------------------------------------------------------

-- Converts an instruction into an Opcode enum value
opcode_enum :: Instr -> String
opcode_enum i = intercalate "_" $ (mnem i) : (ops i)
  where mnem i = raw_mnemonic i
        ops i = map (up . op2type) (operands i)

-- Converts all instructions to Opcode enum values
opcode_enums :: [Instr] -> String
opcode_enums is = to_table is opcode_enum

-- Converts an instruction to arity table row
arity_row :: Instr -> String
arity_row i = show $ length $ operands i

-- Converts all instructions to arity table
arity_table :: [Instr] -> String
arity_table is = to_table is arity_row

-- Creates an entry for a property element
property_elem :: (String, String) -> String
property_elem (t,p) = "Properties::none()" ++ (concat (map (elem t) p))
  where elem _ 'R' = "+Property::MUST_READ"
        elem _ 'r' = "+Property::MAYBE_READ"
        elem t 'Z' = case mem_op t of 
                          True ->  "+Property::MUST_WRITE"
                          False -> "+Property::MUST_WRITE_ZX"
        elem t 'W' = case reg32_op t of 
                          True ->  "+Property::MUST_WRITE_ZX"
                          False -> "+Property::MUST_WRITE"
        elem _ 'w' = "+Property::MAYBE_WRITE"
        elem _ 'U' = "+Property::MUST_UNDEF"
        elem _ 'u' = "+Property::MAYBE_UNDEF"
        elem _ 'I' = ""
        elem t c = error $ "Undefined property type " ++ t ++ ":" ++ [c]

-- Converts an instruction to properties table row
properties_row :: Instr -> String
properties_row i = "{" ++ intercalate "," ps ++ "}"
  where ps = map property_elem $ zip (operands i) (properties i)

-- Converts all instruction to properties table
properties_table is = to_table is properties_row

-- Converts an instruction to type table row
type_row :: Instr -> String
type_row i = "{" ++ intercalate "," [] ++ "}"

-- Converts all instruction to type table
type_table is = to_table is type_row 

-- Converts an instruction to return table row
return_row :: Instr -> String
return_row i = case raw_mnemonic i of
  "IRET" -> "true"
  "IRETD" -> "true"
  "IRETQ" -> "true"
  "RET" -> "true"
  "SYSEXIT" -> "true"
  "SYSRET" -> "true"
  _ -> "false"

-- Converts all instructions to return table
return_table :: [Instr] -> String
return_table is = to_table is return_row 

-- Converts an instruction to nop table row
nop_row :: Instr -> String
nop_row i = case raw_mnemonic i of
  "NOP" -> "true"
  "FNOP" -> "true"
  _ -> "false"

-- Converts all instruction to nop table
nop_table :: [Instr] -> String
nop_table is = to_table is nop_row

-- Converts an instruction to jump table row
jump_row :: Instr -> String
jump_row i = case (is_cond_jump i) || (is_uncond_jump i) of
  True -> "true"
  False -> "false"

-- Converts all instructions to jump table
jump_table :: [Instr] -> String
jump_table is = to_table is jump_row 

-- Converts an instruction to cond_jump table row
cond_jump_row :: Instr -> String
cond_jump_row i = case is_cond_jump i of
  True -> "true"
  False -> "false"

-- Converts all instructions to cond_jump table
cond_jump_table :: [Instr] -> String
cond_jump_table is = to_table is cond_jump_row 

-- Converts an instruction to uncond_jump table row
uncond_jump_row :: Instr -> String
uncond_jump_row i = case is_uncond_jump i of
  True -> "true"
  False -> "false"

-- Converts all instructions to uncond_jump table
uncond_jump_table :: [Instr] -> String
uncond_jump_table is = to_table is uncond_jump_row 

-- Converts an instruction to implicit_read table row
must_read_row :: Instr -> String
must_read_row i = "OpSet::empty()"

-- Converts all instructions to implicit_read table
must_read_table :: [Instr] -> String
must_read_table is = to_table is must_read_row 

-- Converts an instruction to implicit_read table row
maybe_read_row :: Instr -> String
maybe_read_row i = "OpSet::empty()"

-- Converts all instructions to implicit_read table
maybe_read_table :: [Instr] -> String
maybe_read_table is = to_table is maybe_read_row 

-- Converts an instruction to implicit_write table row
must_write_row :: Instr -> String
must_write_row i = "OpSet::empty()"

-- Converts all instructions to implicit_write table
must_write_table :: [Instr] -> String
must_write_table is = to_table is must_write_row

-- Converts an instruction to implicit_write table row
maybe_write_row :: Instr -> String
maybe_write_row i = "OpSet::empty()"

-- Converts all instructions to implicit_write table
maybe_write_table :: [Instr] -> String
maybe_write_table is = to_table is maybe_write_row

-- Converts an instruction to implicit_undef table row
must_undef_row :: Instr -> String
must_undef_row i = "OpSet::empty()"

-- Converts all instructions to implicit_undef table
must_undef_table :: [Instr] -> String
must_undef_table is = to_table is must_undef_row

-- Converts an instruction to implicit_undef table row
maybe_undef_row :: Instr -> String
maybe_undef_row i = "OpSet::empty()"

-- Converts all instructions to implicit_undef table
maybe_undef_table :: [Instr] -> String
maybe_undef_table is = to_table is maybe_undef_row

-- Converts an instruction to a printable at&t mnemonic
att_mnemonic :: Instr -> String
att_mnemonic i = "\"" ++ (att i) ++ "\""

-- Converts all instructions to printable at&t mnemonics
att_mnemonics :: [Instr] -> String
att_mnemonics is = intercalate "\n" $ map (", "++) $ map att_mnemonic is

-------------------------------------------------------------------------------
-- assembler codegen
-------------------------------------------------------------------------------

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
assm_arg_type a
  | mem_op a = "const " ++ (op2type a) ++ "&"
  | otherwise = op2type a

-- Assembler declaration arg list
assm_arg_list :: Instr -> String
assm_arg_list i = intercalate ", " $ map arg $ zip [0..] (operands i)
  where arg (i,a) = (assm_arg_type a) ++ " arg" ++ (show i)

-- Assembler declaration
assm_decl :: Instr -> String
assm_decl i = "void " ++
              (assm_mnemonic i) ++
              "(" ++
              (assm_arg_list i) ++
              ")"

-- Assembler header declaration
assm_header_decl :: Instr -> String
assm_header_decl i = (assm_doxy i) ++ "\n" ++ (assm_decl i) ++ ";"

-- Assembler header declarations
assm_header_decls :: [Instr] -> String
assm_header_decls is = intercalate "\n\n" $ map assm_header_decl is

-- Assembler src definition
assm_src_defn :: Instr -> String
assm_src_defn i = "void Assembler::" ++
                  (assm_mnemonic i) ++
                  "(" ++
                  (assm_arg_list i) ++
                  ") {\n" ++
                  body i ++ 
                  "}"
  where body i = case is_vex_encoded i of
                      True  -> assm_vex_defn i
                      False -> assm_oth_defn i

-- VEX encoded instruction definition
assm_vex_defn :: Instr -> String
assm_vex_defn i = "// VEX-Encoded Instruction: \n" ++
                  "// TODO...\n"

-- Other instruction definition
assm_oth_defn :: Instr -> String
assm_oth_defn i = "// Non-VEX-Encoded Instruction: \n" ++
                  pref1 i ++
                  pref2 i ++ 
                  pref3 i ++
                  pref4 i ++
                  rex i ++
                  opc i ++
                  mod_rm_sib i ++
                  disp_imm i ++
                  "resize();\n"

-- Emits code for Prefix Group 1
-- This doesn't check for the lock prefix which we treat as an opcode
pref1 :: Instr -> String
pref1 i 
  | "F2" `elem` opcode_terms i = "pref_group1(0xf2);\n"
  | "F3" `elem` opcode_terms i = "pref_group1(0xf3);\n"
	| otherwise = "// No Prefix Group 1\n"

-- Emits code for Prefix Group 2
pref2 :: Instr -> String
pref2 i
  | "hint" `elem` operands i = "pref_group2(arg1);\n"
	| otherwise = case mem_index i of
                     (Just idx) -> "pref_group2(arg" ++ (show idx) ++ ");\n"
                     Nothing -> "// No Prefix Group 2\n"

-- Emits code for Prefix Group 3 (operand size override)
pref3 :: Instr -> String
pref3 i 
  | "PREF.66+" `elem` opcode_terms i = "pref_group3();\n"
  | "66" `elem` opcode_terms i = "pref_group3();\n"
  | otherwise = "// No Prefix Group 3\n"

-- Emits code for Prefix Group 4 (address size override)
pref4 :: Instr -> String
pref4 i = case mem_index i of
  (Just idx) -> "pref_group4(arg" ++ (show idx) ++ ");\n"
  Nothing -> "// No Prefix Group 4\n"

-- Explicit MOD/RM and REX args
mod_rm_rex_args :: Instr -> String
mod_rm_rex_args i = case op_en i of
  "MI"   -> "arg0"
  "MR"   -> "arg0,arg1"
  "RM"   -> "arg1,arg0"
  "RMI"  -> "arg1,arg0"
  "RM0"  -> "arg1,arg0"
  "M"    -> "arg0"
  "MRI"  -> "arg0,arg1"
  "RVM"  -> "arg2,arg0"
  "MC"   -> "arg0"
  "M1"   -> "arg0"
  "MRC"  -> "arg0,arg1"
  "RVMI" -> "arg2,arg0"
  "RVMR" -> "arg2,arg0"
  "MVR"  -> "arg0,arg2"
  "XM"   -> "arg1,arg0"
  "VMI"  -> "arg1"
  _      -> ""

-- Default REX arg
def_rex :: Instr -> String
def_rex i
  | "REX.W+" `elem` (opcode_terms i) = ",(uint8_t)0x48"
  | "REX.R+" `elem` (opcode_terms i) = ",(uint8_t)0x44"
  | "REX+"   `elem` (opcode_terms i) = ",(uint8_t)0x40"
  | otherwise = ",(uint8_t)0x00"

-- Emits code for REX Prefix 
rex :: Instr -> String
rex i = case mod_rm_rex_args i of
    "" -> "// No REX Prefix\n"
    _ -> "rex(" ++ (mod_rm_rex_args i) ++ (def_rex i) ++ ");\n"

-- Emits code for opcode bytes
opc :: Instr -> String
opc i = "opcode(" ++ (bytes i) ++ (code i) ++ ");\n"
  where bytes i = intercalate "," $ map (("0x"++).low) (opcode_bytes i)
        idx i = case findIndex is_register_code_arg (operands i) of
                     (Just n) -> show n
                     Nothing -> "[" ++ (intercalate "," (operands i)) ++ "]"
        code i = case is_register_coded i of
                      True -> ",arg" ++ idx i
                      False -> ""

-- Mod R/M SIB digit argument
digit :: Instr -> String
digit i = case find is_digit (opcode_suffix i) of
  (Just ('/':d:[])) -> ",r64s[" ++ [d] ++ "]"
  Nothing -> ""

-- Emits code for mod/rm and sib bytes
mod_rm_sib :: Instr -> String
mod_rm_sib i = case mod_rm_rex_args i of
    "" -> "// No MOD R/M or SIB Bytes\n"
    _ -> "mod_rm_sib(" ++ (mod_rm_rex_args i) ++ (digit i) ++ ");\n"

-- Emits code for displacement or immediate bytes
disp_imm :: Instr -> String
disp_imm i = case disp_imm_index i of
  (Just idx) -> "disp_imm(arg" ++ (show idx) ++ ");\n"
  Nothing -> "// No Displacement/Immediate\n"

-- Assembler src definitions
assm_src_defns :: [Instr] -> String
assm_src_defns is = intercalate "\n\n" $ map assm_src_defn is

-- Assembler switch args
assm_call_arg_list :: Instr -> String
assm_call_arg_list i = intercalate ", " $ map arg $ zip [0..] (operands i)
  where arg (i,a) = "*((" ++ (op2type a) ++ "*)" ++ (elem i) ++ ")"
        elem idx = "instr.get_operand(" ++ (show idx) ++ ")"

-- Assembler switch call
assm_call :: Instr -> String
assm_call i = (assm_mnemonic i) ++ "(" ++ (assm_call_arg_list i) ++ ");"

-- Assembler switch case
assm_case :: Instr -> String
assm_case i = "case " ++ (opcode_enum i) ++ ":\n" ++
              "\t" ++ (assm_call i) ++ "\n" ++
              "\tbreak;"

-- All assembler switch cases
assm_cases :: [Instr] -> String
assm_cases is = intercalate "\n" $ map assm_case is

-------------------------------------------------------------------------------
-- test/ codegen
-------------------------------------------------------------------------------

-- Representative values for each operand type
test_operand :: String -> [String]
test_operand "rl"      = ["%al"] 
test_operand "rh"      = ["%ah"] 
test_operand "rb"      = ["%spl"] 
test_operand "r16"      = ["%ax"]
test_operand "r32"      = ["%eax"]
test_operand "r64"      = ["%rax"]
test_operand "AL"       = ["%al"]
test_operand "CL"       = ["%cl"]
test_operand "AX"       = ["%ax"]
test_operand "DX"       = ["%dx"]
test_operand "EAX"      = ["%eax"]
test_operand "RAX"      = ["%rax"]
test_operand "m8"       = ["(%eax)"]
test_operand "m16"      = ["(%eax)"]
test_operand "m32"      = ["(%eax)"]
test_operand "m64"      = ["(%eax)"]
test_operand "m128"     = ["(%eax)"]
test_operand "m256"     = ["(%eax)"]
test_operand "m16&64"   = ["(%eax)"]
test_operand "m16:16"   = ["*(%eax)"]
test_operand "m16:32"   = ["*(%eax)"]
test_operand "m16:64"   = ["*(%eax)"]
test_operand "m16int"   = ["(%eax)"]
test_operand "m32int"   = ["(%eax)"]
test_operand "m64int"   = ["(%eax)"]
test_operand "m80bcd"   = ["(%eax)"]
test_operand "m32fp"    = ["(%eax)"]
test_operand "m64fp"    = ["(%eax)"]
test_operand "m80fp"    = ["(%eax)"]
test_operand "m2byte"   = ["(%eax)"]
test_operand "m14byte"  = ["(%eax)"]
test_operand "m28byte"  = ["(%eax)"]
test_operand "m94byte"  = ["(%eax)"]
test_operand "m108byte" = ["(%eax)"]
test_operand "m512byte" = ["(%eax)"]
test_operand "imm8"     = ["$0x1"]
test_operand "imm16"    = ["$0x1"]
test_operand "imm32"    = ["$0x1"]
test_operand "imm64"    = ["$0x1"]
test_operand "0"        = ["$0x0"]
test_operand "1"        = ["$0x1"]
test_operand "3"        = ["$0x3"]
test_operand "mm"       = ["%mm0","%mm7"]
test_operand "xmm"      = ["%xmm0"]
test_operand "<XMM0>"   = ["%xmm0"]
test_operand "ymm"     = ["%ymm0"]
test_operand "ST"       = ["%st(0)"]
test_operand "ST(i)"    = ["%st(1)"]
test_operand "rel8"     = ["0x1"]
test_operand "rel32"    = ["0x1"]
test_operand "moffs8"   = ["0x1"]
test_operand "moffs16"  = ["0x1"]
test_operand "moffs32"  = ["0x1"]
test_operand "moffs64"  = ["0x1"]
test_operand "CR0-CR7"  = ["%cr0"]
test_operand "CR8"      = ["%cr8"]
test_operand "DR0-DR7"  = ["%cr0"]
test_operand "Sreg"     = ["%cs"]
test_operand "FS"       = ["%fs"]
test_operand "GS"       = ["%gs"]
-- Below this point are operand types we have introduced
test_operand "p66"      = []
test_operand "pw"       = []
test_operand "far"      = []
test_operand "norexr8"  = []
test_operand "label"    = [".L0"]
test_operand "hint"     = []
test_operand o = error $ "Unrecognized test operand type: \"" ++ o ++ "\""

-- Generates a list of test operands for an instruction
test_operands :: Instr -> [String]
test_operands i = map (intercalate ",") $ cp i
  where cp i = sequence $ map test_operand $ reverse $ operands i

-- Convert an instruction into a list of instances for compilation
test_instr :: Instr -> [String]
test_instr i = map (mn ++) $ test_operands i
  where mn = (att i ++ " ")

-- Convert all instructions into a list of instances for compilation
test_instrs :: [Instr] -> String
test_instrs is = intercalate "\n" $ concat $ map test_instr is

-------------------------------------------------------------------------------
-- Main (read the spreadsheet and write some code)
-------------------------------------------------------------------------------

main :: IO ()		
main = do args <- getArgs
          file <- readFile $ head args

          -- Read Inputs
          let is = insert_missing $
                   remove_ambiguity $
                   fix_rex_rows $
                   fix_ops $ 
                   flatten_instrs $ 
                   x64 $
                   remove_format $ 
                   read_instrs file

          -- Debugging: Check Inputs
          property_arity_check is 
          --mapM_ print $ uniq_op_en is 

          -- Write Code
          writeFile "assembler.decl"    $ assm_header_decls is
          writeFile "assembler.defn"    $ assm_src_defns is
          writeFile "assembler.switch"  $ assm_cases is
          writeFile "arity.table"       $ arity_table is
          writeFile "properties.table"  $ properties_table is
          writeFile "type.table"        $ type_table is
          writeFile "return.table"      $ return_table is
          writeFile "nop.table"         $ nop_table is
          writeFile "jump.table"        $ jump_table is
          writeFile "cond_jump.table"   $ cond_jump_table is
          writeFile "uncond_jump.table" $ uncond_jump_table is
          writeFile "must_read.table"   $ must_read_table is
          writeFile "maybe_read.table"  $ maybe_read_table is
          writeFile "must_write.table"  $ must_write_table is
          writeFile "maybe_write.table" $ maybe_write_table is
          writeFile "must_undef.table"  $ must_undef_table is
          writeFile "maybe_undef.table" $ maybe_undef_table is
          writeFile "opcode.enum"       $ opcode_enums is
          writeFile "opcode.att"        $ att_mnemonics is
          --writeFile "test.s"            $ test_instrs is					
