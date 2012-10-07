import Instr
import System.Environment

assm_switch :: [Instr] -> String
assm_switch is = concat $ map render (tail is)
    where render i = "case " ++ (enum i) ++ ":\n" ++
                     (pref i) ++ (rex_pref i) ++ (opc i) ++
                     (mod_rm i) ++ (disp i) ++ (immed i) ++
                     "\tbreak;\n"

          pref i = case prefix i of
                     (x:[])     -> "\temit(buf_,pos_,0x" ++ x ++ ");\n"
                     (x:y:[])   -> "\temit(buf_,pos_,0x" ++ x ++ ");\n" ++
                                   "\temit(buf_,pos_,0x" ++ y ++ ");\n"
                     (x:y:z:[]) -> "\temit(buf_,pos_,0x" ++ x ++ ");\n" ++
                                   "\temit(buf_,pos_,0x" ++ y ++ ");\n" ++
                                   "\temit(buf_,pos_,0x" ++ z ++ ");\n"
                     _          -> "\t// NO PREFIX\n"

          rex_pref i = "\temit_rex(" ++ (rex_pref_args i) ++ (rex_def (rex i)) ++ ");\n"
          rex_pref_args i = "buf_,pos_" ++ (rm (operand_types i) ((rm_offset i) == 1))
          rm ("M":"R":_) _     = ",i.get_addr(0),(Operand)i.get_gp_reg(1)"
          rm ("M":"S":_) _     = ",i.get_addr(0),(Operand)i.get_xmm_reg(1)"
          rm ("M":"X":_) _     = ",i.get_addr(0),(Operand)i.get_mmx_reg(1)"
          rm ("R":"M":_) _     = ",i.get_addr(1),(Operand)i.get_gp_reg(0)"
          rm ("R":"R":_) True  = ",(Operand)i.get_gp_reg(1),(Operand)i.get_gp_reg(0)"
          rm ("R":"R":_) False = ",(Operand)i.get_gp_reg(0),(Operand)i.get_gp_reg(1)"
          rm ("R":"S":_) True  = ",(Operand)i.get_xmm_reg(1),(Operand)i.get_gp_reg(0)"
          rm ("R":"S":_) False = ",(Operand)i.get_gp_reg(0),(Operand)i.get_xmm_reg(1)"
          rm ("S":"M":_) _      = ",i.get_addr(1),(Operand)i.get_xmm_reg(0)"
          rm ("S":"R":_) True  = ",(Operand)i.get_gp_reg(1),(Operand)i.get_xmm_reg(0)"
          rm ("S":"R":_) False = ",(Operand)i.get_xmm_reg(0),(Operand)i.get_gp_reg(1)"
          rm ("S":"S":_) True  = ",(Operand)i.get_xmm_reg(1),(Operand)i.get_xmm_reg(0)"
          rm ("S":"S":_) False = ",(Operand)i.get_xmm_reg(0),(Operand)i.get_xmm_reg(1)"
          rm ("X":"M":_) _     = ",i.get_addr(1),(Operand)i.get_mmx_reg(0)"
          rm ("X":"X":_) _     = ",(Operand)i.get_mmx_reg(0),(Operand)i.get_mmx_reg(1)"
          rm ("M":_) _         = ",i.get_addr(0)"
          rm ("R":_) _         = ",(Operand)i.get_gp_reg(0)"
          rm ("S":_) _         = ",(Operand)i.get_xmm_reg(0)"
          rm ("X":_) _         = ",(Operand)i.get_mmx_reg(0)"
          rm ("RAX":_) _       = ",(Operand)i.get_gp_reg(0)"
          rm ("EAX":_) _       = ",(Operand)i.get_gp_reg(0)"
          rm ("AX":_)  _       = ",(Operand)i.get_gp_reg(0)"
          rm ("AL":_)  _       = ",(Operand)i.get_gp_reg(0)"
          rm ("CL":_)  _       = ",i.get_gp_reg(0)"
          rm _         _       = ""
          rex_def "" = ""
          rex_def d  = ",0x" ++ d 

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
          opc_delta' ("ST0":_)   True = ",i.get_fp_reg(1)"
          opc_delta' (_:"ST0":_) True = ",i.get_fp_reg(0)"
          opc_delta' ("R":_)     True = ",i.get_gp_reg(0)"
          opc_delta' _ _ = ""

          disp i = disp' (operand_types i) 
          disp' ("L":_) = "\tjumps_[pos_] = i.get_label(0);\n" ++
                          "\tpos_ += 4;\n"
          disp' _       = "\t//NO DISPLACEMENT\n"

          immed i = immed' (operands i) 0
          immed' (w:"I":_:xs) i = case w of
                                    "8"  -> "\temit_byte(buf_,pos_,i.get_imm(" ++ (show i) ++ "));\n"
                                    "16" -> "\temit_word(buf_,pos_,i.get_imm(" ++ (show i) ++ "));\n"
                                    "32" -> "\temit_double(buf_,pos_,i.get_imm(" ++ (show i) ++ "));\n"
                                    "64" -> "\temit_quad(buf_,pos_,i.get_imm(" ++ (show i) ++ "));\n"
          immed' (_:_:_:xs) i = immed' xs (i+1)
          immed' _ i          = "\t//NO IMMEDIATE\n"

-- Write some code!
main :: IO ()
main = do args <- getArgs
          instrs_file <- readFile $ head args
          let instrs = parse_instrs instrs_file
          writeFile "assembler.switch" $ assm_switch instrs
