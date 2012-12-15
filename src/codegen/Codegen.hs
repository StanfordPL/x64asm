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

-------------------------------------------------------------------------------
-- Read Input File
-------------------------------------------------------------------------------

-- Read a row
read_instr :: String -> Instr
read_instr s = let (o:i:m64:m32:d:[]) = splitOn "\t" s in 
                   (Instr (trim o) (trim i) (trim m64) (trim m32) (trim d))

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

-- Remove rows with REX+ prefix and no r8 operands
remove_no_reg_rex :: [Instr] -> [Instr]
remove_no_reg_rex is = filter keep is
  where keep i = ("REX+" `notElem` (opcode_terms i)) ||
                 ("r8" `elem` (operands i))

-- Remove REX+ from opcode terms
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
insert_pref66 :: Instr -> Instr
insert_pref66 i = case r16 || m16 || ax || imm16 of
  True  -> i{opcode=("PREF.66+ " ++ (opcode i))}
  False -> i
  where r16   = "r16"   `elem` (opcode_terms i)
        m16   = "m16"   `elem` (opcode_terms i)
        ax    = "AX"    `elem` (opcode_terms i)
        imm16 = "imm16" `elem` (opcode_terms i)

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

-------------------------------------------------------------------------------
-- Debugging
-------------------------------------------------------------------------------

-- Debugging: Generate a list of unique mnemonics
uniq_mnemonics :: [Instr] -> [String]
uniq_mnemonics is = nub $ map mnemonic is

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
opcode_terms i = splitOn " " (opcode i)

-- Extract mnemonic from instruction
mnemonic :: Instr -> String
mnemonic i = let m = head $ words $ (instruction i) in
  case m of
    "AND" -> "AND_"
    "INT" -> "INT_"
    "NOT" -> "NOT_"
    "OR"  -> "OR_"
    "STD" -> "STD_"
    "XOR" -> "XOR_"
    _     -> m

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
op2type "mem"      = "M" -- Mem (this only appears in LDDQU
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
op2type "mm1"      = "Mm"
op2type "mm2"      = "Mm"
op2type "xmm"      = "Xmm"
op2type "xmm1"     = "Xmm"
op2type "xmm2"     = "Xmm"
op2type "xmm3"     = "Xmm"
op2type "xmm4"     = "Xmm"
op2type "<XMM0>"   = "Xmm0"
op2type "ymm1"     = "Ymm"
op2type "ymm2"     = "Ymm"
op2type "ymm3"     = "Ymm"
op2type "ymm4"     = "Ymm"
op2type "ST"       = "St0"
op2type "ST(0)"    = "St0"
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
op2type o = error $ "Unrecognized operand type: " ++ o

-------------------------------------------------------------------------------
-- Opcode codegen
-------------------------------------------------------------------------------

-- Converts an instruction into an Opcode enum value
opcode_enum :: Instr -> String
opcode_enum i = intercalate "_" $ (mnem i) : (ops i)
  where mnem i = head $ words $ instruction i
        ops i = map (up . op2type) (operands i)

-- Converts all instructions to Opcode enum values
opcode_enums :: [Instr] -> String
opcode_enums is = intercalate "\n" $ map (", "++) $ map opcode_enum is

-------------------------------------------------------------------------------
-- Assembler codegen
-------------------------------------------------------------------------------

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
              (low (mnemonic i)) ++
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
assm_src_defn i = (assm_decl i) ++ 
                  " {\n" ++
                  "}"

-- Assembler src definitions
assm_src_defns :: [Instr] -> String
assm_src_defns is = intercalate "\n\n" $ map assm_src_defn is

-------------------------------------------------------------------------------
-- Main (read the spreadsheet and write some code)
-------------------------------------------------------------------------------

main :: IO ()		
main = do args <- getArgs
          file <- readFile $ head args
          let is = insert_label_variants $
                   insert_pref66s $ 
                   remove_ambiguity $
                   fix_rex_rows $
                   remove_no_reg_rex $ 
                   x64 $
                   flatten_instrs $ 
                   remove_format $ 
                   read_instrs file

          writeFile "assembler.decl" $ assm_header_decls is
          writeFile "assembler.defn" $ assm_src_defns is
          writeFile "opcode.enum"    $ opcode_enums is
          mapM_ print $ map operands is					
