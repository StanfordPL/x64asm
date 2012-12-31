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
        , mode64      :: String
        , mode32      :: String				
        , flag        :: String
        , att         :: String
        , description :: String
        } deriving (Show)

-------------------------------------------------------------------------------
-- Helper Formatting Methods
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

-- Transforms a list of instructions into a comma separated table
to_table :: [Instr] -> (Instr -> String) -> String
to_table is f = intercalate "\n" $ map elem is
  where elem i = ", " ++ (f i) ++ " // " ++ instruction i

-------------------------------------------------------------------------------
-- Read Input File
-------------------------------------------------------------------------------

-- Read a row
read_instr :: String -> Instr
read_instr s = let (o:i:m64:m32:f:a:d:[]) = splitOn "\t" s in 
                   (Instr (trim o) (trim i) (trim m64) (trim m32) 
                          (trim f) (trim a) (trim d))

-- Read all rows
read_instrs :: String -> [Instr]
read_instrs s = map read_instr $ lines s

-------------------------------------------------------------------------------
-- Data Correction (transforms into canonical form)
-------------------------------------------------------------------------------

-- Remove title row and empty rows		
remove_format :: [Instr] -> [Instr]
remove_format is = filter (\x -> keep x) is
    where keep i = (opcode i) /= "" && 
                   (opcode i) /= "Opcode" &&
                   (instruction i) /= "(No mnemonic)"

-- Filters out valid 64-bit mode instructions
x64 :: [Instr] -> [Instr]
x64 is = filter (\x -> (mode64 x) == "V") is

-- Split a disjunct operand into two parts
split_op :: String -> (String,String)
split_op "r/m8" = ("r8","m8")
split_op "r/m16" = ("r16","m16")
split_op "r/m32" = ("r32","m32")
split_op "r/m64" = ("r64","m64")
split_op "reg/m32" = ("r32","m32")
split_op "m14/28byte" = ("m14byte","m28byte")
split_op "m94/108byte" = ("m94byte","m108byte")
split_op "reg/m8" = ("r8","m8")
split_op "reg/m16" = ("r16","m16")
split_op s = let (x1:x2:[]) = splitOn "/" s in (x1,x2)

-- Flatten instructions with disjunct operands
flatten_instr :: Instr -> [Instr]
flatten_instr i = case findIndex (\x -> '/' `elem` x) (operands i) of
  Nothing    -> [i]
  (Just idx) -> [i{instruction=inst1}, i{instruction=inst2}]
    where op = (operands i) !! idx
          op1 = fst $ split_op op
          op2 = snd $ split_op op				
          inst = instruction i	
          inst1 = subRegex (mkRegex op) inst op1
          inst2 = subRegex (mkRegex op) inst op2

-- Flatten all instructions
flatten_instrs :: [Instr] -> [Instr]
flatten_instrs is = concat $ map flatten_instr is

-- Canonical operand version where synonyms are used
canonical_op :: String -> String
canonical_op "mem"   = "m" -- Mem (this only appears in LDDQU
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

-- Remove rows with REX+ prefix and no r8 operands
remove_no_reg_rex :: [Instr] -> [Instr]
remove_no_reg_rex is = filter keep is
  where keep i = ("REX+" `notElem` (opcode_terms i)) ||
                 ("r8" `elem` (operands i))

-- Remove REX+ from opcode terms
-- TODO -- Should I be doing this?
remove_rex :: Instr -> Instr
remove_rex i = i{opcode=o}
  where o = intercalate " " $ filter (\x -> x /= "REX+") (opcode_terms i)

-- Rewrite REX+ instructions to use different NoRexR8 instead of R8
rewrite_rex :: Instr -> Instr
rewrite_rex i = i{instruction=i1}
  where i1 = subRegex (mkRegex "r8") (instruction i) "norexr8"

-- Rename operands for REX+ prefix instructions
fix_rex_row :: Instr -> Instr
fix_rex_row i = case "REX+" `elem` (opcode_terms i) of
  True  -> remove_rex $ rewrite_rex i
  False -> i

-- Fix all rex rows
fix_rex_rows :: [Instr] -> [Instr]
fix_rex_rows is = map fix_rex_row is

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

-- Inserts PREF.66+ for all instructions with 16-bit operands
insert_pref66s :: [Instr] -> [Instr]
insert_pref66s is = map insert_pref66 is

-- Inserts a label variant for instructions that take rel operands
insert_label_variant :: Instr -> [Instr]
insert_label_variant i
  | "rel8" `elem` (operands i) = 
    [i
    ,i{instruction=(subRegex (mkRegex "rel8") (instruction i) "label8")
      ,opcode=(subRegex (mkRegex "cb") (opcode i) "0b")}]
  | "rel32" `elem` (operands i) =
    [i
    ,i{instruction=(subRegex (mkRegex "rel32") (instruction i) "label32")
      ,opcode=(subRegex (mkRegex "cd") (opcode i) "0d")}]	
	| otherwise = [i]

-- Inserts a label variant for all instruction that take rel operands
insert_label_variants :: [Instr] -> [Instr]
insert_label_variants is = concat $ map insert_label_variant is

-- Inserts a hint variant for conditional jumps
insert_hint_variant :: Instr -> [Instr]
insert_hint_variant i = case is_cond_jump i of
  True -> [i,i{instruction=(instruction i ++ ", hint")}]
  False -> [i]

-- Inserts a hint variant for all conditional jumps
insert_hint_variants :: [Instr] -> [Instr]
insert_hint_variants is = concat $ map insert_hint_variant is

-------------------------------------------------------------------------------
-- Debugging
-------------------------------------------------------------------------------

-- Debugging: Generate a list of unique mnemonics
uniq_mnemonics :: [Instr] -> [String]
uniq_mnemonics is = nub $ map raw_mnemonic is

-- Debugging: Generate a list of unique operands
uniq_operands :: [Instr] -> [String]
uniq_operands is = nub $ concat $ map nub $ map operands is 

-- Debugging: Generate a list of unique opcode terms
uniq_opc_terms :: [Instr] -> [String]
uniq_opc_terms is = nub $ concat $ map opcode_terms is

-- Debugging: Generate a list of ambiguous declarations
ambig_decls :: [Instr] -> [[Instr]]
ambig_decls is = filter ambig $ groupBy eq $ sortBy srt is
  where srt x y = compare (assm_decl x) (assm_decl y)
        eq x y = (assm_decl x) == (assm_decl y)	
        ambig x = (length x) > 1

-- Debugging: Pretty print version of ambig_decls
ambig_decls_pretty :: [Instr] -> [String]
ambig_decls_pretty is = map pretty $ ambig_decls is
  where pretty xs = (instruction (head xs)) ++ ":" ++ (concat (map elem xs))
        elem x = "\n\t" ++ (opcode x)

-------------------------------------------------------------------------------
-- Views (transformations on row data into usable forms)
-------------------------------------------------------------------------------

-- Separate opcode terms
opcode_terms :: Instr -> [String]
opcode_terms i = map up $ splitOn " " (opcode i)

-- Is this opcode term a prefix?
-- Does it refer to bytes which must appear before the rex prefix?
-- 66 is a mandatory prefix with different semantics from PREF.66+.
-- Ditto for F2 and F3 which are otherwise REP prefixes.
is_prefix :: String -> Bool
is_prefix "PREF.66+" = True
is_prefix "REX.W+" = True
is_prefix "REX.R+" = True
is_prefix "66" = True
is_prefix "F2" = True
is_prefix "F3" = True
is_prefix _ = False

-- Is this opcode term a suffix?
is_suffix :: String -> Bool
is_suffix ('+':_) = True
is_suffix ('/':_) = True
is_suffix ('I':_) = True
is_suffix _ = False

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

-- Extracts raw mnemonic from instruction
raw_mnemonic :: Instr -> String
raw_mnemonic i = head $ words $ instruction i

-- Extract operands from instruction
operands :: Instr -> [String]
operands i = let x = (splitOn ",") $ concat $ tail $ words (instruction i) in
	filter (\o -> o /= "") x

-- Transform operand notion into type
op2type :: String -> String
op2type "reg"      = "R" -- TODO: This shouble be a general purpose register (32 or 64 bits depending on mode, I thik)
op2type "r8"       = "RexR8"
op2type "r16"      = "R16"
op2type "r32"      = "R32"
op2type "r64"      = "R64"
op2type "AL"       = "Al"
op2type "CL"       = "Cl"
op2type "AX"       = "Ax"
op2type "DX"       = "Dx"
op2type "EAX"      = "Eax" 
op2type "RAX"      = "Rax"
op2type "m"        = "M" -- error "?" -- 16 32 or 64 mem
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
op2type "label8"   = "Label8"
op2type "label32"  = "Label32"
op2type "hint"     = "Hint"
op2type o = error $ "Unrecognized operand type: \"" ++ o ++ "\""

-- Separate cpuid feature flags
flags :: Instr -> [String]
flags i = splitOn " " $ flag i

-- Is this instruction VEX encoded?
is_vex_encoded :: Instr -> Bool
is_vex_encoded i = ("AVX" `elem` fs) || ("F16C" `elem` fs)
  where fs = flags i

-- Is this a conditional jump?
is_cond_jump :: Instr -> Bool
is_cond_jump i = let mn = raw_mnemonic i in
  head mn == 'J' && mn /= "JMP"

-- Is this an unconditional jump?
is_uncond_jump :: Instr -> Bool
is_uncond_jump i = raw_mnemonic i == "JMP"

-- Returns true for memory operands
mem_op :: String -> Bool
mem_op "m"        = True
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

-- Does this instruction have a memory operands?
mem_index :: Instr -> Maybe Int
mem_index i = findIndex mem_op (operands i) 

-------------------------------------------------------------------------------
-- code/ codegen
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

-- Converts an instruction to accessor table row
accessor_row :: Instr -> String
accessor_row i = "{{" ++ intercalate "," [] ++ "}}"

-- Converts all instruction to accessor table
accessor_table is = to_table is accessor_row 

-- Converts an instruction to type table row
type_row :: Instr -> String
type_row i = "{{" ++ intercalate "," [] ++ "}}"

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
read_row :: Instr -> String
read_row i = "OpSet::empty()"

-- Converts all instructions to implicit_read table
read_table :: [Instr] -> String
read_table is = to_table is read_row 

-- Converts an instruction to implicit_write table row
write_row :: Instr -> String
write_row i = "OpSet::empty()"

-- Converts all instructions to implicit_write table
write_table :: [Instr] -> String
write_table is = to_table is write_row

-- Converts an instruction to implicit_def table row
def_row :: Instr -> String
def_row i = "OpSet::empty()"

-- Converts all instructions to implicit_def table
def_table :: [Instr] -> String
def_table is = to_table is def_row 

-- Converts an instruction to implicit_undef table row
undef_row :: Instr -> String
undef_row i = "OpSet::empty()"

-- Converts all instructions to implicit_undef table
undef_table :: [Instr] -> String
undef_table is = to_table is undef_row

-------------------------------------------------------------------------------
-- io/ codegen
-------------------------------------------------------------------------------

-- Converts an instruction to a printable at&t mnemonic
att_mnemonic :: Instr -> String
att_mnemonic i = "\"" ++ (att i) ++ "\""

-- Converts all instructions to printable at&t mnemonics
att_mnemonics :: [Instr] -> String
att_mnemonics is = intercalate "\n" $ map (", "++) $ map att_mnemonic is

-------------------------------------------------------------------------------
-- assembler/ codegen
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

-- Assembler declaration arg list
assm_arg_list :: Instr -> String
assm_arg_list i = intercalate ", " $ map arg $ zip [0..] (operands i)
  where arg (i,a) = (op2type a) ++ " arg" ++ (show i)

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
                  modrm_sib i ++
                  disp_imm i

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

-- Emits code for REX Prefix 
-- TODO -- Finish this!
rex :: Instr -> String
rex i = "// TODO - REX Prefix\n"

-- Emits code for opcode bytes
opc :: Instr -> String
opc i = "opcode(" ++ (bytes i) ++ (code i) ++ ");\n"
  where bytes i = intercalate "," $ map (("0x"++).low) (opcode_bytes i)
        code i = case is_register_coded i of
                      True -> ",Operand(100)"
                      False -> ""

-- Emits code for mod/rm and sib bytes
modrm_sib :: Instr -> String
modrm_sib i = "// TODO - mod r/m sib bytes\n"

-- Emits code for displacement or immediate bytes
disp_imm :: Instr -> String
disp_imm i = "// TODO - Displacement/Immediate bytes\n"

-- Assembler src definitions
assm_src_defns :: [Instr] -> String
assm_src_defns is = intercalate "\n\n" $ map assm_src_defn is

-------------------------------------------------------------------------------
-- test/ codegen
-------------------------------------------------------------------------------

-- Representative values for each operand type
test_operand :: String -> [String]
test_operand "reg"      = ["%eax"] 
test_operand "r8"       = ["%al","%spl"]
test_operand "r16"      = ["%ax"]
test_operand "r32"      = ["%eax"]
test_operand "r64"      = ["%rax"]
test_operand "AL"       = ["%al"]
test_operand "CL"       = ["%cl"]
test_operand "AX"       = ["%ax"]
test_operand "DX"       = ["%dx"]
test_operand "EAX"      = ["%eax"]
test_operand "RAX"      = ["%rax"]
test_operand "m"        = ["(%eax)"]
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
test_operand "label8"   = [".L0"]
test_operand "label32"  = [".L0"]
test_operand "hint"     = []
test_operand o = error $ "Unrecognized operand type: \"" ++ o ++ "\""

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
          let is = insert_hint_variants $
                   insert_label_variants $
                   insert_pref66s $ 
                   remove_ambiguity $
                   fix_rex_rows $
                   remove_no_reg_rex $ 
                   x64 $
                   fix_ops $ 
                   flatten_instrs $ 
                   remove_format $ 
                   read_instrs file

          writeFile "assembler.decl"    $ assm_header_decls is
          writeFile "assembler.defn"    $ assm_src_defns is
          writeFile "arity.table"       $ arity_table is
          writeFile "accessor.table"    $ accessor_table is
          writeFile "type.table"        $ type_table is
          writeFile "return.table"      $ return_table is
          writeFile "jump.table"        $ jump_table is
          writeFile "cond_jump.table"   $ cond_jump_table is
          writeFile "uncond_jump.table" $ uncond_jump_table is
          writeFile "read.table"        $ read_table is
          writeFile "write.table"       $ write_table is
          writeFile "def.table"         $ def_table is
          writeFile "undef.table"       $ undef_table is
          writeFile "opcode.enum"       $ opcode_enums is
          writeFile "opcode.att"        $ att_mnemonics is
          writeFile "test.s"            $ test_instrs is					
          mapM_ print $ map opcode_bytes is

