import Data.Char
import Data.List
import Instr
import System.Environment

-- Emits an argument list for an instruction
assm_args :: Instr -> String
assm_args i = concat $ intersperse "," $ 
              map args' $ zip (map to_type (operands i)) [0..]
    where args' (t,i) = t ++ " arg" ++ (show i)

-- Emits a declaration for an instruction
assm_decl :: [Instr] -> String
assm_decl is = concat $ map render $ tail is
    where render i = "void " ++ (att i) ++ "(" ++ (assm_args i) ++ ");\n"

-- Appropriate orderings of first and second arguments, based on type
codegen_args :: Instr -> String
codegen_args i = cga (operand_types i) (flipped i)
    where cga ("M":"I":_) _  = ",arg0"
          cga ("M":_:_) _    = ",arg0,(Operand)arg1"
          cga (_:"M":_) _    = ",arg1,(Operand)arg0"
          cga (_:"O":_) _    = ",(Operand)arg0"
          cga ("O":_:_) _    = ",(Operand)arg1"

          cga ("R":"RAX":_) _ = ",(Operand)arg0"
          cga ("R":"EAX":_) _ = ",(Operand)arg0"
          cga ("R":"AX":_) _ = ",(Operand)arg0"
          cga ("R":"AL":_) _ = ",(Operand)arg0"
          cga ("F":"ST0":_) _ = ",(Operand)arg0"

          cga ("AL":"R":_) _   = ",(Operand)arg1"
          cga ("AX":"R":_) _   = ",(Operand)arg1"
          cga ("EAX":"R":_) _  = ",(Operand)arg1"
          cga ("RAX":"R":_) _  = ",(Operand)arg1"
          cga ("ST0":"F":_) _  = ",(Operand)arg1"

          cga ("AL":_:_) _   = ",(Operand)arg0"
          cga ("AX":_:_) _   = ",(Operand)arg0"
          cga ("EAX":_:_) _  = ",(Operand)arg0"
          cga ("RAX":_:_) _  = ",(Operand)arg0"
          cga (_:"I":_) _    = ",(Operand)arg0"
          cga (_:_:_) True   = ",(Operand)arg1,(Operand)arg0"
          cga (_:_:_) False  = ",(Operand)arg0,(Operand)arg1"
          cga ("I":_) _ = ""
          cga ("M":_) _ = ",arg0"

          cga (_:_) _ = ",(Operand) arg0"
          cga _ _ = ""

-- Emit mem prefix
emit_mem_prefix :: Instr -> String
emit_mem_prefix i = case mem_index i of
  3   -> "\t// NO MEM PREFIX\n"
  idx -> case (elem "O" (operand_types i)) of 
    True  -> "\t// NO MEM PREFIX\n"
    False -> "\temit_mem_prefix(buf_,pos_,arg" ++ (show idx) ++ ");\n"

-- Emit prefix
emit_prefix :: Instr -> String
emit_prefix i = case prefix i of
  (x:[])     -> "\temit_prefix(buf_,pos_,0x" ++ x ++ ");\n"
  (x:y:[])   -> "\temit_prefix(buf_,pos_,0x" ++ x ++ ",0x" ++ y ++ ");\n"
  (x:y:z:[]) -> "\temit_prefix(buf_,pos_,0x" ++ x ++ ",0x" ++ y ++ ",0x" ++ z ++ ");\n"
  _ -> "\t// NO PREFIX\n"

-- Emit REX Prefix
emit_rex_prefix :: Instr -> String
emit_rex_prefix i = "\temit_rex(buf_,pos_" ++ 
                       (codegen_args i) ++ 
											 (def (rex i)) ++ 
											 (is_8bit (operands i)) ++ 
											 ");\n"
  where def "" = ",0x00"
        def d = ",0x" ++ d
        is_8bit (("8","R",_):("8","R",_):_) = ",R8(arg0|arg1)"
        is_8bit (("8","R",_):_) = ",arg0"
        is_8bit (_:("8","R",_):_) = ",arg1" 
        is_8bit _ = ",r_null"

-- Emit Opcode
opcode_args :: Instr -> String
opcode_args i = oca $ opcode i
    where oca (x:[]) = arg x
          oca (x:y:[]) = (arg x) ++ (arg y)
          oca (x:y:z:[]) = (arg x) ++ (arg y) ++ (arg z)
          oca  _ = error "This should never happen (opcode)" 
          arg x = ",(unsigned char)0x" ++ x

opcode_delta :: Instr -> String
opcode_delta i = ocd (operand_types i) (reg_code i)
    where ocd ("ST0":_)     True = ",(Operand)arg1"
          ocd (_:"ST0":_)   True = ",(Operand)arg0"
          ocd ("R":_)       True = ",(Operand)arg0"
          ocd ("AL":"R":_)  True = ",(Operand)arg1"
          ocd ("AX":"R":_)  True = ",(Operand)arg1"
          ocd ("EAX":"R":_) True = ",(Operand)arg1"
          ocd ("RAX":"R":_) True = ",(Operand)arg1"
          ocd _ _ = ""

emit_opcode :: Instr -> String
emit_opcode i = "\temit_opcode(" ++ args ++ ");\n"
    where args = "buf_,pos_" ++ (opcode_args i) ++ (opcode_delta i) 

-- EMit Mod R/M Byte
digit :: Instr -> String
digit i = digit' $ reg_field i
    where digit' "" = ""
          digit' d = "," ++ d

emit_mod_rm :: Instr -> String
emit_mod_rm i = "\temit_mod_rm(" ++ args ++ ");\n"
    where args = "buf_,pos_" ++ (codegen_args i) ++ (digit i)
          
-- Emit Displacement
emit_disp :: Instr -> String
emit_disp i = disp' (operand_types i) 
    where disp' ("L":_)   = "\tjumps_[pos_] = arg0;\n" ++
                            "\tpos_ += 4;\n"
          disp' (_:"O":_) = "\temit_imm(buf_,pos_,(Imm64)arg1);\n"
          disp' ("O":_)   = "\temit_imm(buf_,pos_,(Imm64)arg0);\n"
          disp' _         = "\t// NO DISPLACEMENT\n"

-- Emit Immediate
emit_immed :: Instr -> String
emit_immed i = immed' (operands i)
    where immed' ((w,"I",_):_) = "\temit_imm(buf_,pos_,(Imm" ++ w ++ ")arg0);\n"
          immed' (_:(w,"I",_):_) = "\temit_imm(buf_,pos_,(Imm" ++ w ++ ")arg1);\n"
          immed' (_:_:(w,"I",_):_) = "\temit_imm(buf_,pos_,(Imm" ++ w ++ ")arg2);\n"
          immed' _ = "\t// NO IMMEDIATE\n"

-- Generates a definition for an instruction		
assm_defn :: [Instr] -> String
assm_defn is = concat $ map render $ tail is
    where render i = "void Assembler::" ++ (att i) ++ "(" ++ (assm_args i) ++ "){\n" ++ 
                     (emit_mem_prefix i) ++
                     (emit_prefix i) ++
                     (emit_rex_prefix i) ++
                     (emit_opcode i) ++
                     (emit_mod_rm i) ++
                     (emit_disp i) ++
                     (emit_immed i) ++ 
                     "}\n"

-- Generates a switch statement based on opcode enum
assm_switch :: [Instr] -> String
assm_switch is = concat $ map render $ tail is
    where render i = "case " ++ (to_enum i) ++ ":\n\t" ++ 
                        (att i) ++ "(" ++ (args i) ++ ");\n\t" ++
                        "break;\n"
          args i = concat $ intersperse "," $ 
                   map args' $ zip (map to_type (operands i)) [0..]
          args' (t,i) = "(" ++ t ++ ")i.get_operand(" ++ (show i) ++ ")"

-- Write some code!
main :: IO ()
main = do args <- getArgs
          instrs_file <- readFile $ head args
          let instrs = parse_instrs instrs_file

          writeFile "assembler.decl" $ assm_decl instrs
          writeFile "assembler.defn" $ assm_defn instrs
          writeFile "assembler.switch" $ assm_switch instrs
