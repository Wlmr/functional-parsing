module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Skip |
    Comment |
    Read' String |
    Write Expr.T |
    While Expr.T Statement |
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Block [Statement]
    deriving Show

comment = (accept "--" # require "\n") >-> buildComment
buildComment _ = Comment

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = (accept "skip" # require ";") >-> buildSkip
buildSkip _ = Skip

read'= accept "read " -# word #- require ";" >-> buildRead'
buildRead' = Read'

block = accept "begin" -# iter statement #- require "end" >-> buildBlock
buildBlock = Block

if' = accept "if" -# Expr.parse #- require "then" # (statement #- require "else" # statement) >-> buildIf'
buildIf' (condition,(th,el)) = If condition th el

while = accept "while" -# Expr.parse #- require "do" # statement >-> buildWhile
buildWhile (ex,st) = While ex st

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite = Write

statement :: Parser Statement
statement = assignment ! skip ! read' ! block ! if' ! while ! write

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If cond thenStmts elseStmts: stmts) dict input =
    if Expr.value cond dict > 0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Assignment var expr : stmts) dict input =
  exec stmts (Dictionary.insert (var, Expr.value expr dict) dict) input
exec (Skip : stmts) dict input = exec stmts dict input
exec (Block block : stmts) dict input = exec (block++stmts) dict input
exec loop@(While cond body : after) dict input =
    if Expr.value cond dict > 0
        then exec (body : loop) dict input
        else exec after dict input
exec (Read' var : stmts) dict (input:rest) =
    exec stmts (Dictionary.insert (var, input) dict) rest
exec (Write expr : stmts) dict input =
    Expr.value expr dict : exec stmts dict input

instance Parse Statement where
  parse = statement
  toString = toString' 0

tabsize = 4

toString' :: Int -> Statement -> String
toString' indent (Assignment var expr) =
    replicate indent ' ' ++ var ++ ":=" ++ toString expr ++ ";\n"
toString' indent Skip = replicate indent ' ' ++ "skip;\n"
toString' indent (Block block) =
    replicate indent ' ' ++ "begin\n"
    ++ concat (map (toString' (indent+tabsize)) block)
    ++ replicate indent ' ' ++ "end\n"
toString' indent (If cond th el) =
    replicate indent ' ' ++ "if " ++ Expr.toString cond ++ " then\n"
    ++ toString' (indent+tabsize) th ++ replicate indent ' '
    ++ "else\n" ++ toString' (indent+tabsize) el
toString' indent (While cond body) =
    replicate indent ' ' ++ "while " ++ Expr.toString cond ++ " do\n"
    ++ toString' (indent+tabsize) body
toString' indent (Read' var) = replicate indent ' ' ++ "read " ++ var ++ ";\n"
toString' indent (Write expr) =
    replicate indent ' ' ++ "write " ++ Expr.toString expr ++ ";\n"
