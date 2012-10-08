import Data.Char
import Data.List
import Instr
import System.Environment

-- Helper functions for writing carrays
array :: String -> String -> [String] -> String
array t name body = "vector<" ++ t ++ "> " ++ 
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

-- Writes out the Instruction enum
opcode_enum :: [Instr] -> String
opcode_enum is = "enum OpcodeVal {\n" ++ 
                 "  " ++ (enum (head is)) ++ " = 0\n, " ++
                 (concat (intersperse "\n, " (map enum (tail is)))) ++
                 "\n, NUM_OPCODE_VALS = " ++ (show (length is)) ++
                 "\n, OPCODE_VAL_NULL = NUM_OPCODE_VALS" ++
                 "\n};"

-- Writes out the Instruction opcode range
opcode_range :: [Instr] -> String
opcode_range = instr_array "Opcode" "Opcode::range_" enum

-- Writes out the att encoding for each instruction
opcodes :: [Instr] -> String
opcodes = instr_array "const char*" "opcodes_" render
    where render i = "\"" ++ (att i) ++ "\""

-- Writes out the arity for each instruction
write_arity :: [Instr] -> String
write_arity = instr_array "size_t" "Opcode::arity_" (show . arity)

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

types :: [Instr] -> String
types = instr_array "array<Type, 3>" "Opcode::type_" type_render

-- Writes out width info for each instruction
width_render :: Instr -> String
width_render i = inline_array "BitWidth" (render' (operands i) 0 (mem_size_or i))
    where render' (_:"M":_:xs)  i o = (case o of 
                                         True ->  ["DOUBLE"]
                                         False -> ["QUAD"]) ++ (render' xs (i+1) o)
          render' (_:"O":_:xs)    i o = ["QUAD"]     ++ (render' xs (i+1) o)
          render' (w:"R":_:xs)    i o = [(width' w)] ++ (render' xs (i+1) o)
          render' (_:"X":_:xs)    i o = ["QUAD"]     ++ (render' xs (i+1) o)
          render' (_:"F":_:xs)    i o = ["QUAD"]     ++ (render' xs (i+1) o)
          render' (_:"S":_:xs)    i o = ["OCT"]      ++ (render' xs (i+1) o)
          render' (w:"I":_:xs)    i o = [(width' w)] ++ (render' xs (i+1) o)
          render' (_:"L":_:xs)    i o = ["FIXED"]    ++ (render' xs (i+1) o)
          render' (_:"AL":_:xs)   i o = ["LOW"]      ++ (render' xs (i+1) o)
          render' (_:"AX":_:xs)   i o = ["WORD"]     ++ (render' xs (i+1) o)
          render' (_:"EAX":_:xs)  i o = ["DOUBLE"]   ++ (render' xs (i+1) o)
          render' (_:"RAX":_:xs)  i o = ["QUAD"]     ++ (render' xs (i+1) o)
          render' (_:"CL":_:xs)   i o = ["LOW"]      ++ (render' xs (i+1) o)
          render' (_:"ST0":_:xs)  i o = ["QUAD"]     ++ (render' xs (i+1) o)
          render' _ 3 _ = []
          render' _ i o = ["BIT_WIDTH_NULL"] ++ (render' [] (i+1) o)

          width' "8"   = "LOW"					
          width' "16"  = "WORD"				
          width' "32"  = "DOUBLE"					
          width' "64"  = "QUAD"					
          width' "128" = "OCT"					
          width' _     = "BIT_WIDTH_VAL_NULL"

widths :: [Instr] -> String
widths = instr_array "array<BitWidth, 3>" "Opcode::width_" width_render

-- Writes out opcode signatures (this is gross)
signatures :: [Instr] -> String
signatures is = "map<tuple<string, array<Type, 3>, array<BitWidth, 3>>, Opcode> " ++
                "signatures_\n{ " ++ body ++ "\n};\n"
    where body     = (concat (intersperse "\n, " (map render is)))
          render i = "{ " ++ 
                     "tuple<string, array<Type, 3>, array<BitWidth, 3>>(\"" ++ 
                     (att i) ++ "\", " ++ 
                     (type_render i) ++ ", " ++ 
                     (width_render i) ++ "), " ++ 
					           (enum i) ++ 
                     "}"

-- Writes out the location of the first memory operand
write_mem_offset :: [Instr] -> String
write_mem_offset = instr_array "size_t" "Opcode::mem_offset_" wmo
    where wmo i = case mem_offset i of
                    (Just mo) -> show mo 
                    _ -> "16"

-- Writes out the location of the first operand which is read
read_offset :: [Instr] -> String
read_offset = instr_array "size_t" "Opcode::read_offset_" render
    where render  i = (render' (operands i) 0) 
          render' (_:_:"R":_) i = (show i)
          render' (_:_:"X":_) i = (show i)
          render' (_:_:_:xs)  i = render' xs (i+1)
          render' _           i = (show i)	

-- Writes out whether the instruction writes a register or not
writes_reg :: [Instr] -> String
writes_reg = instr_array "bool" "Opcode::writes_reg_" render
    where render  i = (render' (operands i))
          render' (_:"R":"W":_) = "true";
          render' (_:"R":"X":_) = "true";
          render' (_:"S":"W":_) = "true";
          render' (_:"S":"X":_) = "true";
          render' (_:"X":"W":_) = "true";
          render' (_:"X":"X":_) = "true";
          render' (_:"F":"W":_) = "true";
          render' (_:"F":"X":_) = "true";
          render' (_:_:_:xs)    = render' xs
          render' _             = "false"

-- Writes out whether the instruction is a conditional jump
is_cond_jump :: [Instr] -> String
is_cond_jump = instr_array "bool" "Opcode::is_cond_jump_" render
    where render i = map toLower (show (cond_jump i))

-- Writes out whether the instruction is an unconditional jump
is_uncond_jump :: [Instr] -> String
is_uncond_jump = instr_array "bool" "Opcode::is_uncond_jump_" render
    where render i = map toLower (show (uncond_jump i))

-- Writes out whether the instruction is a jump
is_jump :: [Instr] -> String
is_jump = instr_array "bool" "Opcode::is_jump_" render 
    where render i = (map toLower (show (jump i))) 

-- Writes out whether the instruction uses the mem_size override flag
mem_or :: [Instr] -> String
mem_or = instr_array "bool" "Opcode::mem_size_or_" render
    where render i = map toLower (show (mem_size_or i))

-- Functions for emitting register masks
reg_mask :: String -> (Instr -> [String]) -> [Instr] -> String
reg_mask name fxn = instr_array "RegSet" ("Opcode::" ++ name) render
    where render i = "RegSet()" ++ 
                     (concat (map render' (fxn i)))
          render' r = ".set_cond(" ++ (map toLower r) ++")"

reg_mask2 :: String -> (Instr -> [String]) -> (Instr -> [String]) -> [Instr] -> String
reg_mask2 name fxn1 fxn2 = instr_array "RegSet" ("Opcode::" ++ name) render
    where render i = "RegSet()" ++ 
                     (concat (map render_gp   (fxn1 i))) ++ 
										 (concat (map render_cond (fxn2 i)))
          render_gp "AL"  = ".set_gp(rax, LOW)"
          render_gp "AH"  = ".set_gp(rax, HIGH)"
          render_gp "AX"  = ".set_gp(rax, WORD)"
          render_gp "DX"  = ".set_gp(rdx, WORD)"
          render_gp "SP"  = ".set_gp(rsp, WORD)"
          render_gp "EAX" = ".set_gp(rax, DOUBLE)"
          render_gp "ECX" = ".set_gp(rcx, DOUBLE)"
          render_gp "EDX" = ".set_gp(rdx, DOUBLE)"
          render_gp "ESP" = ".set_gp(rsp, DOUBLE)"
          render_gp "RAX" = ".set_gp(rax, QUAD)"
          render_gp "RCX" = ".set_gp(rcx, QUAD)"
          render_gp "RDX" = ".set_gp(rdx, QUAD)"
          render_gp "RSP" = ".set_gp(rsp, QUAD)"					
          render_gp "ST0" = "" -- TODO
          render_gp "TOP" = "" -- TODO
          render_gp _     = "CODEGEN ERROR SHOULD NOT COMPILE"
					-- ...
					-- Add to this as interesting values appear in x64.csv
          render_cond r   = ".set_cond(" ++ (map toLower r) ++ ")"

-- Write some code!
main :: IO ()
main = do args <- getArgs
          instrs_file <- readFile $ head args
          let instrs = parse_instrs instrs_file

          writeFile  "opcode.enum"   $ opcode_enum     instrs
          writeFile  "opcode.sigs"   $ signatures      instrs

          writeFile  "opcode.static" $ opcode_range     instrs
          appendFile "opcode.static" $ opcodes          instrs
          appendFile "opcode.static" $ write_arity      instrs
          appendFile "opcode.static" $ types            instrs
          appendFile "opcode.static" $ widths           instrs
          appendFile "opcode.static" $ write_mem_offset instrs
          appendFile "opcode.static" $ read_offset      instrs
          appendFile "opcode.static" $ writes_reg       instrs
          appendFile "opcode.static" $ is_cond_jump     instrs
          appendFile "opcode.static" $ is_uncond_jump   instrs
          appendFile "opcode.static" $ is_jump          instrs
          appendFile "opcode.static" $ mem_or           instrs
          appendFile "opcode.static" $ reg_mask2 "implicit_read_set_"  
					           implicit_reads cond_read instrs
          appendFile "opcode.static" $ reg_mask2 "implicit_write_set_" 
					           implicit_writes cond_write instrs
          appendFile "opcode.static" $ reg_mask "implicit_undef_set_"     
                     cond_undef instrs
