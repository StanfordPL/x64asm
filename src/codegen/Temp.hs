import Data.Char
import Data.List
import Data.List.Split
import System.Environment
import System.IO -- Debugging
import Text.Regex
import Text.Regex.TDFA

-- Instruction Row Type
data Instr =
  Instr { opcode      :: String
        , instruction :: String
        , description :: String
        } deriving (Show)

-- Remove leading/trailing whitespace
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

-- To lower case
low :: String -> String
low s = map toLower s

-- Parse a row
read_instr :: String -> Instr
read_instr s = let (o:i:d:[]) = splitOn "\t" s in 
                   (Instr (trim o) (trim i) (trim d))

-- Parse all rows
read_instrs :: String -> [Instr]
read_instrs s = map read_instr $ rows
    where rows = filter (\x -> '\t' `elem` x) $ lines s

-- Remove title row and empty rows		
remove_format :: [Instr] -> [Instr]
remove_format is = filter (\x -> keep x) is
    where keep i = (opcode i) /= "" && 
                   (opcode i) /= "Opcode" &&
                   (instruction i) /= "(No mnemonic)"

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
  (Just idx) -> [(Instr o inst1 d), (Instr o inst2 d)]
    where o = opcode i
          d = description i					
          op = (operands i) !! idx
          op1 = fst $ split_op op
          op2 = snd $ split_op op				
          inst = instruction i	
          inst1 = subRegex (mkRegex op) inst op1
          inst2 = subRegex (mkRegex op) inst op2

-- Flatten all instructions
flatten_instrs :: [Instr] -> [Instr]
flatten_instrs is = concat $ map flatten_instr is

-- Extract mnemonic from instruction
mnemonic :: Instr -> String
mnemonic i = head $ words $ (instruction i)

-- Debugging: Generate a list of unique mnemonics
uniq_mnemonics :: [Instr] -> [String]
uniq_mnemonics is = nub $ map mnemonic is

-- Extract operands from instruction
operands :: Instr -> [String]
operands i = (splitOn ",") $ concat $ tail $ words (instruction i)

-- Debugging: Generate a list of unique operands
uniq_operands :: [Instr] -> [String]
uniq_operands is = nub $ concat $ map nub $ map operands is 

-- Assembler doxygen comment
assm_doxy :: Instr -> String
assm_doxy i = "/** " ++ (description i) ++ " */"

-- Assembler declaration
assm_decl :: Instr -> String
assm_decl i = "void " ++
              (low (mnemonic i)) ++
              "(" ++
              -- todo
              ")"

-- Assembler header declaration
assm_header_decl :: Instr -> String
assm_header_decl i = (assm_doxy i) ++ "\n" ++ (assm_decl i) ++ ";\n"

-- Assembler header declarations
assm_header_decls :: [Instr] -> String
assm_header_decls is = concat $ map assm_header_decl is

-- Transform operand notion into type
op2type :: String -> String
op2type "reg"      = error "?"
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
op2type "mem"      = error "?"
op2type "m"        = error "?" -- 16 32 or 64 mem
op2type "m8"       = "M8"
op2type "m16"      = "M16"
op2type "m32"      = "M32"
op2type "m64"      = "M64"
op2type "m128"     = "M128"
op2type "m256"     = "M256"
op2type "m16&16"   = "M16M16"
op2type "m16&32"   = "M16M32"
op2type "m16&64"   = "M16M64"
op2type "m32&32"   = "M32M32"
op2type "m16:16"   = "MPtr1616" 
op2type "m16:32"   = "MPtr1632"
op2type "m16:64"   = "MPtr1664"
op2type "m16int"   = "M16Int"
op2type "m32int"   = "M32Int"
op2type "m64int"   = "M64Int"
op2type "m80dec"   = error "?"
op2type "m80bcd"   = error "?"
op2type "m32fp"    = "M32Fp"
op2type "m64fp"    = "M64Fp"
op2type "m80fp"    = "M80Fp"
op2type "m2byte"   = error "?"
op2type "m14byte"  = error "?"
op2type "m28byte"  = error "?"
op2type "m94byte"  = error "?"
op2type "m108byte" = error "?"
op2type "m512byte" = error "?"
op2type "imm8"     = "Imm8"
op2type "imm16"    = "Imm16"
op2type "imm32"    = "Imm32"
op2type "imm64"    = "Imm64"
op2type "0"        = "One"
op2type "1"        = "Two"
op2type "3"        = "Three"
op2type "mm"       = "Mm"
op2type "mm1"      = "Mm"
op2type "mm2"      = "Mm"
op2type "xmm"      = "Xmm"
op2type "xmm1"     = "Xmm"
op2type "xmm2"     = "Xmm"
op2type "xmm3"     = "Xmm"
op2type "<XMM0>"   = "Xmm0"
op2type "ymm1"     = "Ymm"
op2type "ymm2"     = "Ymm"
op2type "ymm3"     = "Ymm"
op2type "xmm4"     = "Ymm"
op2type "ymm4"     = "Ymm"
op2type "ST"       = "St0"
op2type "ST(0)"    = "St0"
op2type "ST(i)"    = "St"
op2type "rel8"     = "Rel8"
op2type "rel16"    = "Rel16"
op2type "rel32"    = "Rel32"
op2type "ptr16:16" = "Ptr1616"
op2type "ptr16:32" = "Ptr1632"
op2type "moffs8"   = "Moffs8"
op2type "moffs16"  = "Moffs16"
op2type "moffs32"  = "Moffs32"
op2type "moffs64"  = "Moffs64"
op2type "CR0-CR7"  = error "?"
op2type "CR8"      = error "?"
op2type "DR0-DR7"  = error "?"
op2type "Sreg"     = "Sreg"
op2type "DS"       = "Ds"
op2type "ES"       = "Es"
op2type "SS"       = "Ss"
op2type "FS"       = "Fs"
op2type "GS"       = "Gs"
op2type "CS"       = "Cs"
op2type o = error $ "Unrecognized operand type: " ++ o

main :: IO ()		
main = do args <- getArgs
          file <- readFile $ head args
          let is = flatten_instrs $ remove_format $ read_instrs file

          writeFile "assembler.decl" $ assm_header_decls is
          mapM_ print $ uniq_operands is
