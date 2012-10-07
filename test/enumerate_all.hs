
import Instr
import Data.Char
import Data.List
import Data.Maybe
import System.Environment

data ArgType = I | R | M | S | F | X | AL | AX | EAX | RAX | CL | CX | ECX | RCX deriving Show
data Argument = Arg Int ArgType deriving Show

-- Turn a list of strings into a list of arguments
parse_args :: ([String],Bool) -> Maybe [[String]]
parse_args ([],_) = Just []
parse_args ((a:b:c:rest),mem) = let newarg = parse_arg a b c mem in 
                                case newarg of
                                    Nothing -> Nothing
                                    Just newarg' -> case (parse_args (rest,mem)) of
                                                     Just x -> Just (x ++ [arg_to_reglist newarg'])
                                                     Nothing -> Nothing
parse_args _ = Nothing

-- Turn a description of three strings into an argument
parse_arg :: String -> String -> String -> Bool -> Maybe Argument
parse_arg a b _ mem = let w = read a :: Int in
                  case (b, mem) of
                    ("AL", _)  -> Just (Arg w AL)
                    ("AX", _)  -> Just (Arg w AX)
                    ("EAX", _) -> Just (Arg w EAX)
                    ("RAX", _) -> Just (Arg w RAX)
                    ("CL", _)  -> Just (Arg w CL)
                    ("CX", _)  -> Just (Arg w CX)
                    ("ECX", _) -> Just (Arg w ECX)
                    ("RCX", _) -> Just (Arg w RCX)
                    ("R", _)   -> Just (Arg w R)
                    ("M", true)  -> Just (Arg 32 M)
                    ("M", false) -> Just (Arg 64 M)
                    ("I", _) -> Just (Arg w I)
                    ("S", _) -> Just (Arg w S)
                    ("F", _) -> Just (Arg w F)
                    ("X", _) -> Just (Arg w X)
                    _ -> Nothing

-- For each argument, get a list of strings that it could represent
gp :: Int -> [String]
gp 8  = ["%al", "%cl", "%bl", "%dl"]--, "%ah", "%ch", "%bh", "%dh"]
gp 16 = ["%ax", "%cx", "%dx", "%bx", "%sp", "%bp", "%si", "%di"]
gp 32 = ["%eax", "%ecx", "%ebx", "%edx", "%esp", "%ebp", "%esi", "%edi"]
gp 64 = ["%rax", "%rcx", "%rbx", "%rdx", "%rsp", "%rbp", "%rsi", "%rdi"]
gp_no_sp 32 = ["%eax", "%ecx", "%ebx", "%edx", "%ebp", "%esi", "%edi"]
gp_no_sp 64 = ["%rax", "%rcx", "%rbx", "%rdx", "%rbp", "%rsi", "%rdi"]

memtest = ["", "0x00", "0x01", "-0x01", "0x7fffffff", "-0x7fffffff"]

immediate :: Int -> [String]
immediate 8 =  ["$0x0", "$0x7f", "$-0x7f" ] --"$0xff", "$0x7f", "$0x80"]
immediate 16 = ["$0x0", "$0x7fff", "$-0x7fff" ] --"$0xffff", "$0x7fff", "$0x8000"]
immediate 32 = ["$0x0", "$0x7fffffff", "$-0x7fffffff" ]--"$0xffffffff", "$0x7fffffff", "$0x80000000"]
immediate 64 = ["$0x0", "$0x7fffffffffffffff", "$-0x7fffffffffffffff" ] --"$0xffffffffffffffff", "$0x7fffffffffffffff", "$0x8000000000000000"]


arg_to_reglist :: Argument -> [String]
arg_to_reglist (Arg w AL) = [ "%al" ]
arg_to_reglist (Arg w AX) = [ "%ax" ]
arg_to_reglist (Arg w EAX) = [ "%eax" ]
arg_to_reglist (Arg w RAX) = [ "%rax" ]
arg_to_reglist (Arg w CL) = [ "%cl" ]
arg_to_reglist (Arg w CX) = [ "%cx" ]
arg_to_reglist (Arg w ECX) = [ "%ecx" ]
arg_to_reglist (Arg w RCX) = [ "%rcx" ]
arg_to_reglist (Arg w S) = [ "%xmm0", "%xmm7", "%xmm15" ]
arg_to_reglist (Arg w F) = [ "%st0" , "%st7"  ]
arg_to_reglist (Arg w X) = [ "%mm0", "%mm7" ]
arg_to_reglist (Arg w R) = gp w
arg_to_reglist (Arg w I) = immediate w
arg_to_reglist (Arg w M) = [ disp ++ "(" ++ base ++ ")" | base <- gp w, disp <- memtest ] ++
                           [ disp ++ "(" ++ base ++ "," ++ index ++ ")" | base <- gp w, disp <- memtest, index <- gp_no_sp w] ++ 
                           [ disp ++ "(" ++ base ++ "," ++ index ++ "," ++ scale ++ ")" | 
                               base <- gp w, disp <- memtest, index <- gp_no_sp w, scale <- ["1", "2", "4", "8"] ]

-- Print one instruction

print_prefix_list :: String -> [[String]] -> IO()
print_prefix_list prefix [] = putStr (prefix ++ "\n")
print_prefix_list prefix (first_list:tail_lists) = case (first_list, length tail_lists) of
                                                   ((first_item:first_tail),0) ->
                                                      print_prefix_list (prefix ++ " " ++ first_item) tail_lists >>
                                                      print_prefix_list prefix (first_tail:tail_lists)
                                                   ((first_item:first_tail),_) ->
                                                      print_prefix_list (prefix ++ " " ++ first_item ++ ",") tail_lists >>
                                                      print_prefix_list prefix (first_tail:tail_lists)
                                                   ([],_) -> putStr ""

--memsize_override = true  -> 32-bit
--memsize_override = false -> 64-bit
print_instruction :: Instr -> IO()
--print_instruction i = print i
print_instruction (Instr att _ _ _ _ _ _ _ ops _ _ _ _ _ memsize_override) = 
    let op_list = parse_args (ops, memsize_override) in
    case op_list of
        Nothing -> putStr "" --putStr ("Error in test script!  Couldn't figure out opcode " ++ att ++ " with " ++ (show ops) ++ "\n")
        Just x -> print_prefix_list att x
        
--        putStr (att ++ " " ++ (show x) ++ "\n")

print_instruction_list :: [Instr] -> IO()
print_instruction_list [] = putStr ""
print_instruction_list (x:xs) = print_instruction x >> print_instruction_list xs

-- Write some code!
main :: IO ()
main = do args <- getArgs
          instrs_file <- readFile $ head args
          let instrs = parse_instrs instrs_file
          print_instruction_list instrs
