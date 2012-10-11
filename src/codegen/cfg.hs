import Data.Char
import Data.List
import Instr
import System.Environment

-- Helper functions for writing carrays
array :: String -> String -> [String] -> String
array t name body = "const vector<" ++ t ++ "> " ++ 
                    name ++ "{{\n  " ++
                    (concat (intersperse "\n, " body)) ++
                    "\n}};\n"

inline_array :: String -> [String] -> String
inline_array t body = "array<" ++ t ++ "," ++ (show (length body)) ++ ">" ++
                      "{{" ++ (concat (intersperse "," body)) ++ "}}"

instr_array :: String -> String -> (Instr -> String) -> [Instr] -> String
instr_array t name f is = array t name body
    where body = map f' is 
          f' i = (f i) ++ " // " ++ (enum i)

-- Writes out the att encoding for each instruction
opcodes :: [Instr] -> String
opcodes = instr_array "const char*" "opcodes_" render
    where render i = "\"" ++ (att i) ++ "\""

-- Writes out the Instruction enum
opcode_enum :: [Instr] -> String
opcode_enum is = "enum OpcodeVal : Operand {\n  " ++ 
                 (concat (intersperse "\n, " (map enum' (zip is [0..])))) ++
                 "\n};"
    where enum' (i,idx) = (enum i) ++ " = DEF(" ++ (show idx) ++ "," ++
                            (a i)  ++ "," ++ 
                            (t i)  ++ "," ++ (w  i) ++ "," ++ (m  i) ++ "," ++ 
                            (r i)  ++ "," ++ 
                            (j i)  ++ "," ++ (uj i) ++ "," ++ (cj i) ++ "," ++ 
                            (mi i) ++ "," ++ (fr i) ++ "," ++ (nw i) ++ ")"
          a  i = show $ arity i
          t  i = concat $ intersperse "," $ 
                 take 3 $ (map to_type (operand_types i)) ++ 
                 ["TYPE_NULL","TYPE_NULL","TYPE_NULL"]
          w  i = concat $ intersperse "," $ 
                 take 3 $ (map to_width (operand_widths i)) ++ 
                 ["BIT_WIDTH_NULL", "BIT_WIDTH_NULL", "BIT_WIDTH_NULL"]
          m  i = concat $ intersperse "," $ 
                 take 3 $ (map to_mod (operand_mods i)) ++ 
                 ["MODIFIER_NULL", "MODIFIER_NULL", "MODIFIER_NULL"]
          r  i = tf $ ret i
          j  i = tf $ jump i
          uj i = tf $ uncond_jump i
          cj i = tf $ cond_jump i
          mi i = show $ mem_index i
          fr i = show $ first_read i
          nw i = show $ num_writes i

          to_width "8"   = "LOW"
          to_width "16"  = "WORD"
          to_width "32"  = "DOUBLE"
          to_width "64"  = "QUAD"
          to_width "128" = "OCT"
          to_width _ = error "Unrecognized width!"

          to_type "M"   = "ADDR"
          to_type "F"   = "FP_REG"
          to_type "R"   = "GP_REG"
          to_type "I"   = "IMM"
          to_type "L"   = "LABEL"
          to_type "X"   = "MMX_REG"
          to_type "O"   = "OFFSET"
          to_type "S"   = "XMM_REG"
          to_type "AL"  = "RAX_ONLY"
          to_type "AX"  = "RAX_ONLY" 
          to_type "EAX" = "RAX_ONLY" 
          to_type "RAX" = "RAX_ONLY" 
          to_type "CL"  = "RCX_ONLY"
          to_type "ST0" = "ST0_ONLY"
          to_type _ = error "Unrecognized type!"

          to_mod "R" = "READ"
          to_mod "W" = "WRITE"
          to_mod "X" = "READ_WRITE"
          to_mod _   = "MODIFIER_NULL" 

          tf True = "1"
          tf False = "0"

-- Writes out the opcode domain
opcode_domain :: [Instr] -> String
opcode_domain = instr_array "Opcode" "Opcode::domain_" enum 

-- Writes out type info for each instruction
type_render :: Instr -> String
type_render i = inline_array "Type" (render' (operands i) 0)
    where render' (_:"M":_:xs)   i = ["ADDR"]     ++ (render' xs (i+1))
          render' (_:"O":_:xs)   i = ["OFFSET"]   ++ (render' xs (i+1))
          render' (_:"R":_:xs)   i = ["GP_REG"]   ++ (render' xs (i+1))
          render' (_:"X":_:xs)   i = ["MMX_REG"]  ++ (render' xs (i+1))
          render' (_:"F":_:xs)   i = ["FP_REG"]   ++ (render' xs (i+1))
          render' (_:"S":_:xs)   i = ["XMM_REG"]  ++ (render' xs (i+1))
          render' (_:"I":_:xs)   i = ["IMM"]      ++ (render' xs (i+1))
          render' (_:"L":_:xs)   i = ["LABEL"]    ++ (render' xs (i+1))
          render' (_:"AL":_:xs)  i = ["RAX_ONLY"] ++ (render' xs (i+1))
          render' (_:"AX":_:xs)  i = ["RAX_ONLY"] ++ (render' xs (i+1))
          render' (_:"EAX":_:xs) i = ["RAX_ONLY"] ++ (render' xs (i+1))
          render' (_:"RAX":_:xs) i = ["RAX_ONLY"] ++ (render' xs (i+1))
          render' (_:"CL":_:xs)  i = ["RCX_ONLY"] ++ (render' xs (i+1))
          render' (_:"ST0":_:xs) i = ["ST0_ONLY"] ++ (render' xs (i+1))
          render' _ 3 = []
          render' _ i = ["TYPE_NULL"] ++ (render' [] (i+1))

-- Writes out width info for each instruction
width_render :: Instr -> String
width_render i = inline_array "BitWidth" (render' (operands i) 0)
    where render' (w:"M":_:xs)   i = [(width' w)] ++ (render' xs (i+1))
          render' (_:"O":_:xs)   i = ["QUAD"]     ++ (render' xs (i+1))
          render' (w:"R":_:xs)   i = [(width' w)] ++ (render' xs (i+1))
          render' (_:"X":_:xs)   i = ["QUAD"]     ++ (render' xs (i+1))
          render' (_:"F":_:xs)   i = ["QUAD"]     ++ (render' xs (i+1))
          render' (_:"S":_:xs)   i = ["OCT"]      ++ (render' xs (i+1))
          render' (w:"I":_:xs)   i = [(width' w)] ++ (render' xs (i+1))
          render' (_:"L":_:xs)   i = ["FIXED"]    ++ (render' xs (i+1))
          render' (_:"AL":_:xs)  i = ["LOW"]      ++ (render' xs (i+1))
          render' (_:"AX":_:xs)  i = ["WORD"]     ++ (render' xs (i+1))
          render' (_:"EAX":_:xs) i = ["DOUBLE"]   ++ (render' xs (i+1))
          render' (_:"RAX":_:xs) i = ["QUAD"]     ++ (render' xs (i+1))
          render' (_:"CL":_:xs)  i = ["LOW"]      ++ (render' xs (i+1))
          render' (_:"ST0":_:xs) i = ["QUAD"]     ++ (render' xs (i+1))
          render' _ 3 = []
          render' _ i = ["BIT_WIDTH_NULL"] ++ (render' [] (i+1))

          width' "8"   = "LOW"					
          width' "16"  = "WORD"				
          width' "32"  = "DOUBLE"					
          width' "64"  = "QUAD"					
          width' "128" = "OCT"					
          width' _     = "BIT_WIDTH_VAL_NULL"

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
                      (enum i) ++ "}"
          classes = nub $ map att is

-- Functions for emitting register masks
reg_mask :: String -> (Instr -> [String]) -> [Instr] -> String
reg_mask name fxn = instr_array "RegSet" ("Opcode::" ++ name) render
    where render i = "RegSet()" ++ 
                     (concat (map render' (fxn i)))
          render' r = ".set(" ++ (map toLower r) ++")"

reg_mask2 :: String -> (Instr -> [String]) -> (Instr -> [String]) -> [Instr] -> String
reg_mask2 name fxn1 fxn2 = instr_array "RegSet" ("Opcode::" ++ name) render
    where render i = "RegSet()" ++ 
                     (concat (map render_gp   (fxn1 i))) ++ 
										 (concat (map render_cond (fxn2 i)))
          render_gp "AL"  = ".set(rax, LOW)"
          render_gp "AH"  = ".set(rax, HIGH)"
          render_gp "AX"  = ".set(rax, WORD)"
          render_gp "DX"  = ".set(rdx, WORD)"
          render_gp "SP"  = ".set(rsp, WORD)"
          render_gp "EAX" = ".set(rax, DOUBLE)"
          render_gp "ECX" = ".set(rcx, DOUBLE)"
          render_gp "EDX" = ".set(rdx, DOUBLE)"
          render_gp "ESP" = ".set(rsp, DOUBLE)"
          render_gp "RAX" = ".set(rax, QUAD)"
          render_gp "RBX" = ".set(rbx, QUAD)"
          render_gp "RCX" = ".set(rcx, QUAD)"
          render_gp "RDX" = ".set(rdx, QUAD)"
          render_gp "RSP" = ".set(rsp, QUAD)"					
          render_gp "RBP" = ".set(rbp, QUAD)"
          render_gp "RSI" = ".set(rsi, QUAD)"
          render_gp "RDI" = ".set(rdi, QUAD)"
          render_gp "ST0" = "" -- TODO
          render_gp "TOP" = "" -- TODO
          render_gp _     = error "Unrecognized operand!"
					-- ...
					-- Add to this as interesting values appear in x64.csv
          render_cond r   = ".set(" ++ (map toLower r) ++ ")"

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
