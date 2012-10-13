import Data.Char
import Data.List
import Instr
import System.Environment

-- Generates a unique name for an instruction
assm_fxn :: Instr -> String
assm_fxn i = map toLower $ enum i

-- Generates an argument list for an instruction
assm_args :: Instr -> String
assm_args i = concat $ intersperse "," $ (args' (operand_types i) 0)
    where args' ("F":xs) i = ("FpReg arg"  ++ (show i)):[] ++ (args' xs (i+1))
          args' ("L":xs) i = ("Label arg"  ++ (show i)):[] ++ (args' xs (i+1))
          args' ("M":xs) i = ("Addr arg"   ++ (show i)):[] ++ (args' xs (i+1))
          args' ("O":xs) i = ("Offset arg" ++ (show i)):[] ++ (args' xs (i+1))
          args' ("R":xs) i = ("GpReg arg"  ++ (show i)):[] ++ (args' xs (i+1))
          args' ("S":xs) i = ("XmmReg arg" ++ (show i)):[] ++ (args' xs (i+1))
          args' ("X":xs) i = ("MmxReg arg" ++ (show i)):[] ++ (args' xs (i+1))
          args' ("I":xs) i = ("Imm arg"    ++ (show i)):[] ++ (args' xs (i+1))

          args' ("AL":xs)  i = ("GpReg arg" ++ (show i)):[] ++ (args' xs (i+1))
          args' ("AX":xs)  i = ("GpReg arg" ++ (show i)):[] ++ (args' xs (i+1))
          args' ("EAX":xs) i = ("GpReg arg" ++ (show i)):[] ++ (args' xs (i+1))
          args' ("RAX":xs) i = ("GpReg arg" ++ (show i)):[] ++ (args' xs (i+1))
          args' ("CL":xs)  i = ("GpReg arg" ++ (show i)):[] ++ (args' xs (i+1))
          args' ("ST0":xs) i = ("FpReg arg" ++ (show i)):[] ++ (args' xs (i+1))

          args' (t:xs) _ = error "Unexpected operand type!"

          args' _ _ = []

-- Generates a declaration for an instruction
assm_decl :: [Instr] -> String
assm_decl is = concat $ map render $ tail is
    where render i = "void " ++ (assm_fxn i) ++ "(" ++ (assm_args i) ++ ");\n"

-- Generates a definition for an instruction		
assm_defn :: [Instr] -> String
assm_defn is = concat $ map render $ tail is
    where render i = "void Assembler::" ++ (assm_fxn i) ++ "(" ++ (assm_args i) ++ "){\n" ++ (body i) ++ "}\n"		
          body i = (mem_pref i) ++ (pref i) ++ (rex_pref i) ++ (opc i) ++ (mod_rm i) ++ (disp i) ++ (immed i)

          mem_pref i = case mem_index i of
                       3 -> "\t// NO MEM PREFIX\n"
                       i -> "\temit_mem_prefix(buf_,pos_,arg" ++ (show i) ++ ");\n"

          pref i = case prefix i of
                     (x:[])     -> "\temit_prefix(buf_,pos_,0x" ++ x ++ ");\n"
                     (x:y:[])   -> "\temit_prefix(buf_,pos_,0x" ++ x ++ ",0x" ++ y ++ ");\n"
                     (x:y:z:[]) -> "\temit_prefix(buf_,pos_,0x" ++ x ++ ",0x" ++ y ++ ",0x" ++ z ++ ");\n"
                     _ -> "\t// NO PREFIX\n"

          rex_pref i = "\temit_rex(" ++ (rex_pref_args i) ++ 
                                        (rex_def (rex i)) ++
                                        (is_8bit (operands i)) ++
                                        ");\n"
          rex_pref_args i = "buf_,pos_" ++ (rm (operand_types i) ((rm_offset i) == 1))
          rm ("M":"I":_) _  = ",arg0"
          rm ("M":_:_) _    = ",arg0,(Operand)arg1"
          rm (_:"M":_) _    = ",arg1,(Operand)arg0"
          rm (_:"O":_) _    = ",(Operand)arg0"
          rm ("O":_:_) _    = ",(Operand)arg1"

          rm ("R":"RAX":_) _ = ",(Operand)arg0"
          rm ("R":"EAX":_) _ = ",(Operand)arg0"
          rm ("R":"AX":_) _ = ",(Operand)arg0"
          rm ("R":"AL":_) _ = ",(Operand)arg0"

          rm ("AL":"R":_) _   = ",(Operand)arg1"
          rm ("AX":"R":_) _   = ",(Operand)arg1"
          rm ("EAX":"R":_) _  = ",(Operand)arg1"
          rm ("RAX":"R":_) _  = ",(Operand)arg1"

          rm ("AL":_:_) _   = ",(Operand)arg0"
          rm ("AX":_:_) _   = ",(Operand)arg0"
          rm ("EAX":_:_) _  = ",(Operand)arg0"
          rm ("RAX":_:_) _  = ",(Operand)arg0"
          rm (_:"I":_) _    = ",(Operand)arg0"
          rm (_:_:_) True   = ",(Operand)arg1,(Operand)arg0"
          rm (_:_:_) False  = ",(Operand)arg0,(Operand)arg1"
          rm ("I":_) _ = ""
          rm ("M":_) _ = ",arg0"
          rm (_:_) _ = ",(Operand) arg0"
          rm _ _ = ""
          rex_def "" = ",0x00"
          rex_def d  = ",0x" ++ d 
          is_8bit ("8":"R":_:"8":"R":_) = ",GpReg(arg0|arg1)"
          is_8bit ("8":"R":_) = ",arg0"
          is_8bit (_:_:_:"8":"R":_) = ",arg1" 
          is_8bit _ = ",gp_null"

          mod_rm i = "\temit_mod_rm(" ++ (mod_rm_args i) ++ ");\n"
          mod_rm_args i = rex_pref_args i ++ (digit (reg_field i))
          digit "" = ""
          digit d = "," ++ d
          
          opc i = "\temit_opcode(buf_,pos_" ++ (opc_args (opcode i)) ++ (opc_delta i) ++ ");\n"
          opc_args (x:[]) = ",0x" ++ x
          opc_args (x:y:[]) = ",0x" ++ x ++ ",0x" ++ y
          opc_args (x:y:z:[]) = ",0x" ++ x ++ ",0x" ++ y ++ ",0x" ++ z
          opc_args _ = error "This should never happen"
          opc_delta i = opc_delta' (operand_types i) (reg_code i) 
          opc_delta' ("ST0":_)   True = ",arg1"
          opc_delta' (_:"ST0":_) True = ",arg0"
          opc_delta' ("R":_)     True = ",arg0"
          opc_delta' ("AL":"R":_)    True = ",arg1"
          opc_delta' ("AX":"R":_)    True = ",arg1"
          opc_delta' ("EAX":"R":_)    True = ",arg1"
          opc_delta' ("RAX":"R":_)    True = ",arg1"
          opc_delta' _ _ = ""

          disp i = disp' (operand_types i) 
          disp' ("L":_)   = "\tjumps_[pos_] = arg0;\n" ++
                            "\tpos_ += 4;\n"
          disp' (_:"O":_) = "\temit_quad(buf_,pos_,(Operand) arg1);\n"
          disp' ("O":_)   = "\temit_quad(buf_,pos_,(Operand) arg0);\n"
          disp' _         = "\t// NO DISPLACEMENT\n"

          immed i = immed' (operands i) 0
          immed' (w:"I":_:xs) i = case w of
                                    "8"  -> "\temit_byte(buf_,pos_,arg"   ++ (show i) ++ ");\n"
                                    "16" -> "\temit_word(buf_,pos_,arg"   ++ (show i) ++ ");\n"
                                    "32" -> "\temit_double(buf_,pos_,arg" ++ (show i) ++ ");\n"
                                    "64" -> "\temit_quad(buf_,pos_,arg"   ++ (show i) ++ ");\n"
                                    _ -> error "This should never happen!" 
          immed' (_:_:_:xs) i = immed' xs (i+1)
          immed' _ i          = "\t// NO IMMEDIATE\n"

-- Generates a switch statement based on opcode enum
assm_switch :: [Instr] -> String
assm_switch is = concat $ map render $ tail is
    where render i = "case " ++ (enum i) ++ ":\n\t" ++ (assm_fxn i) ++ "(" ++ (args i) ++ ");\n\tbreak;\n"
          args i = concat $ intersperse "," $ (args' (operand_types i) 0)
          args' ("F":xs) i = ("i.get_fp_reg(" ++ (show i) ++ ")"):[] ++ (args' xs (i+1))
          args' ("L":xs) i = ("i.get_label(" ++ (show i) ++ ")"):[] ++ (args' xs (i+1))
          args' ("M":xs) i = ("i.get_addr(" ++ (show i) ++ ")"):[] ++ (args' xs (i+1))
          args' ("O":xs) i = ("i.get_offset(" ++ (show i) ++ ")"):[] ++ (args' xs (i+1))
          args' ("R":xs) i = ("i.get_gp_reg(" ++ (show i) ++ ")"):[] ++ (args' xs (i+1))
          args' ("S":xs) i = ("i.get_xmm_reg(" ++ (show i) ++ ")"):[] ++ (args' xs (i+1))
          args' ("X":xs) i = ("i.get_mmx_reg(" ++ (show i) ++ ")"):[] ++ (args' xs (i+1))
          args' ("I":xs) i = ("i.get_imm(" ++ (show i) ++ ")"):[] ++ (args' xs (i+1))

          args' ("AL":xs)  i = ("i.get_gp_reg(" ++ (show i) ++ ")"):[] ++ (args' xs (i+1))
          args' ("AX":xs)  i = ("i.get_gp_reg(" ++ (show i) ++ ")"):[] ++ (args' xs (i+1))
          args' ("EAX":xs) i = ("i.get_gp_reg(" ++ (show i) ++ ")"):[] ++ (args' xs (i+1))
          args' ("RAX":xs) i = ("i.get_gp_reg(" ++ (show i) ++ ")"):[] ++ (args' xs (i+1))
          args' ("CL":xs)  i = ("i.get_gp_reg(" ++ (show i) ++ ")"):[] ++ (args' xs (i+1))
          args' ("ST0":xs) i = ("i.get_fp_reg(" ++ (show i) ++ ")"):[] ++ (args' xs (i+1))

          args' (t:xs) _ = error "Unexpected operand type!"

          args' _ _ = []

-- Write some code!
main :: IO ()
main = do args <- getArgs
          instrs_file <- readFile $ head args
          let instrs = parse_instrs instrs_file
          writeFile "assembler.decl" $ assm_decl instrs
          writeFile "assembler.defn" $ assm_defn instrs
          writeFile "assembler.switch" $ assm_switch instrs
