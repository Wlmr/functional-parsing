module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Skip |
    Read' String |
    Write Expr.T |
    Comment String |
    While Expr.T Statement |
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Block [Statement]
    deriving Show


assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = (accept "skip" # require ";") >-> buildSkip
buildSkip _ = Skip

block = accept "begin" -# iter parse #- require "end" >-> buildBlock
buildBlock = Block

if' = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf'
buildIf' ((condition,th),el) = If condition th el

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (cond, stmt) = While cond stmt

read' = accept "read" -# word #- require ";" >-> buildRead
buildRead = Read'

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite = Write

comment = accept "--" -# commentLine  #- require "\n" >-> buildComment
buildComment = Comment


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
exec (Comment str : stmts) dict input =
    exec stmts dict input

indent :: Int -> String
indent i = replicate (2 * i) ' '

statement = assignment ! skip ! if' ! while ! read' ! write ! block ! comment

toString' :: Int -> Statement -> String
toString' i (Assignment var expr)      = indent i ++ var ++ " := " ++ Expr.toString expr ++ ";\n"
toString' i (If cond ifs elses)  = indent i ++ "if " ++ Expr.toString cond ++ " then\n" ++ toString' (i + 1) ifs ++ indent i ++ "else\n" ++ toString' (i + 1) elses
toString' i (While cond statements)         = indent i ++ "while " ++ Expr.toString cond ++ " do\n" ++ toString' (i + 1) statements
toString' i (Block statements)              = indent i ++ "begin\n" ++ concatMap (toString' (i + 1)) statements ++ indent i ++ "end\n"
toString' i (Read' var)                = indent i ++ "read " ++ var ++ ";\n"
toString' i (Write expr)               = indent i ++ "write " ++ Expr.toString expr ++ ";\n"
toString' i Skip                       = indent i ++ "skip;\n"
toString' i (Comment comment)          = indent i ++ "-- " ++ comment ++ "\n"



instance Parse Statement where
  parse = statement
  toString = toString' 0
