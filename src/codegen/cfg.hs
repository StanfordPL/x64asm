import Data.Char
import Data.List
import Instr
import System.Environment

-- Writes an array
array :: String -> String -> [String] -> String
array t name body = "const vector<" ++ t ++ "> " ++ 
                    name ++ "{{\n  " ++
                    (concat (intersperse "\n, " body)) ++
                    "\n}};\n"

-- Writes an array with opcode enum annotations on each line
comment_array :: String -> String -> (Instr -> String) -> [Instr] -> String
comment_array t name f is = array t name body
    where body = map f' is 
          f' i = (f i) ++ " // " ++ (to_enum i)

-- Writes out the Instruction enum
opcode_enum :: [Instr] -> String
opcode_enum is = "enum OpcodeVal : Operand {\n  " ++ 
                 (concat (intersperse "\n, " (map enum' (zip is [0..])))) ++
                 "\n};"
    where enum' (i,idx) = (to_enum i) ++ " = DEF(" ++ (show idx) ++ "," ++
                          (a i) ++ "," ++ 
                          (t i) ++ "," ++ (m  i) ++ "," ++ 
                          (r i) ++ "," ++ 
                          (j i) ++ "," ++ (uj i) ++ "," ++ (cj i) ++ "," ++ 
                          (mi i) ++ "," ++ (fr i) ++ "," ++ (nw i) ++ ")"
          a  i = show $ arity i
          t  i = concat $ intersperse "," $ 
                 take 3 $ (map to_type_enum (operands i)) ++ 
                 ["TYPE_NULL","TYPE_NULL","TYPE_NULL"]
          m  i = concat $ intersperse "," $ 
                 take 3 $ (map to_mod_enum (operands i)) ++ 
                 ["MODIFIER_NULL", "MODIFIER_NULL", "MODIFIER_NULL"]
          r  i = tf $ ret i
          j  i = tf $ jump i
          uj i = tf $ uncond_jump i
          cj i = tf $ cond_jump i
          mi i = show $ mem_index i
          fr i = show $ first_read i
          nw i = show $ num_writes i

          tf True = "1"
          tf False = "0"

-- Writes out the att encoding for each instruction
opcodes :: [Instr] -> String
opcodes = comment_array "const char*" "opcodes_" render
    where render i = "\"" ++ (att i) ++ "\""

-- Writes out the opcode domain
opcode_domain :: [Instr] -> String
opcode_domain = comment_array "Opcode" "Opcode::domain_" to_enum 

-- Writes out a single-line array
inline_array :: String -> [String] -> String
inline_array t body = "array<" ++ t ++ "," ++ (show (length body)) ++ ">" ++
                      "{{" ++ (concat (intersperse "," body)) ++ "}}"

-- Writes out type info for each instruction
type_render :: Instr -> String
type_render i = inline_array "Type" $ take 3 $
                (map to_type_enum_DEPRECATED (operands i)) ++ 
                ["TYPE_NULL","TYPE_NULL","TYPE_NULL"]

-- Writes out width info for each instruction 
width_render :: Instr -> String
width_render i = inline_array "BitWidth" $ take 3 $
                 (map to_width_enum_DEPRECATED (operands i)) ++
                 ["BIT_WIDTH_NULL","BIT_WIDTH_NULL","BIT_WIDTH_NULL"]

-- Writes out opcode signatures (this is gross)
signatures :: [Instr] -> String
signatures is = "map<string, vector<tuple< array<Type,3>, array<BitWidth,3>, OpcodeVal>>> " ++
                "signatures_\n{ " ++ body ++ "\n};\n"
    where body     = (concat (intersperse "\n, " (map render classes)))
          render c = "{ \"" ++ c ++ "\", " ++
                     "vector<tuple<array<Type,3>,array<BitWidth,3>,OpcodeVal>>{\n" ++
                     (concat (intersperse "\n, " (map render' (instrs c)))) ++
                     "}}"

          instrs c = filter (\i -> att i == c) is
          render' i = "    tuple<array<Type,3>, array<BitWidth,3>,OpcodeVal>{\n" ++
                      (type_render i) ++ ", " ++
                      (width_render i) ++ ", " ++
                      (to_enum i) ++ "}"
          classes = nub $ map att is

-- To int conversions
reg2int :: String -> String
reg2int "al"  = "(R8)0"
reg2int "ax"  = "(R16)0"
reg2int "eax" = "(R32)0"
reg2int "rax" = "(R64)0"
reg2int "rcx" = "(R64)1"
reg2int "dx"  = "(R16)2"
reg2int "edx" = "(R32)2"
reg2int "rdx" = "(R64)2"
reg2int "rbx" = "(R64)3"
reg2int "rsp" = "(R64)4"
reg2int "rbp" = "(R64)5"
reg2int "rsi" = "(R64)6"
reg2int "rdi" = "(R64)7"
reg2int "af"  = "(CondReg)0"
reg2int "cf"  = "(CondReg)1"
reg2int "of"  = "(CondReg)2"
reg2int "pf"  = "(CondReg)3"
reg2int "sf"  = "(CondReg)4"
reg2int "zf"  = "(CondReg)5"
reg2int _ = error "Unrecognized register type!"

-- Functions for emitting register masks
reg_mask :: String -> (Instr -> [String]) -> [Instr] -> String
reg_mask name fxn = comment_array "RegSet" ("Opcode::" ++ name) render
    where render i = "RegSet()" ++ 
                     (concat (map render' (fxn i)))
          render' r = ".set(" ++ (reg2int r) ++")"

reg_mask2 :: String -> (Instr -> [String]) -> (Instr -> [String]) -> [Instr] -> String
reg_mask2 name fxn1 fxn2 = comment_array "RegSet" ("Opcode::" ++ name) render
    where render i = "RegSet()" ++ 
                     (concat (map render' (fxn1 i))) ++ 
										 (concat (map render_cond (fxn2 i)))
          render' "st0" = "" -- "TODO - Add floating point to reg set!"
          render' "top" = "" -- "TODO - Add floating point to reg set!"
          render' r = ".set(" ++ (reg2int r) ++ ")"
          render_cond r   = ".set(" ++ (reg2int r) ++ ")"

-- Write some code!
main :: IO ()
main = do args <- getArgs
          instrs_file <- readFile $ head args
          let instrs = parse_instrs instrs_file

          writeFile  "opcode.enum"   $ opcode_enum   instrs
          writeFile  "opcode.sigs"   $ signatures    instrs
          writeFile  "opcode.char"   $ opcodes       instrs
          writeFile  "opcode.domain" $ opcode_domain instrs

          writeFile  "opcode.implicit" $ reg_mask2 "implicit_read_set_"  
					           implicit_reads cond_read instrs
          appendFile "opcode.implicit" $ reg_mask2 "implicit_write_set_" 
					           implicit_writes cond_write instrs
          appendFile "opcode.implicit" $ reg_mask "implicit_undef_set_"     
                     cond_undef instrs
