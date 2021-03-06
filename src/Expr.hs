module Expr(Expr, T, parse, fromString, value, toString) where

{-
   An expression of type Expr is a representation of an arithmetic expression
   with integer constants and variables. A variable is a string of upper-
   and lower case letters. The following functions are exported

   parse :: Parser Expr
   fromString :: String -> Expr
   toString :: Expr -> String
   value :: Expr -> Dictionary.T String Int -> Int

   parse is a parser for expressions as defined by the module Parser.
   It is suitable for use in parsers for languages containing expressions
   as a sublanguage.

   fromString expects its argument to contain an expression and returns the
   corresponding Expr.

   toString converts an expression to a string without unneccessary
   parentheses and such that fromString (toString e) = e.

   value e env evaluates e in an environment env that is represented by a
   Dictionary.T Int.
-}
import Prelude hiding (return, fail)
import Parser hiding (T)
import Data.Maybe
import qualified Dictionary

data Expr = Num Integer | Var String | Add Expr Expr
       | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Exp Expr Expr
         deriving Show

type T = Expr

var, num, factor, term, expr :: Parser Expr

term', expr' :: Expr -> Parser Expr

var = word >-> Var

num = number >-> Num

mulOp = lit '*' >-> const  Mul !
        lit '/' >-> const  Div !
        lit '^' >-> const Exp

addOp = lit '+' >-> const  Add !
        lit '-' >-> const  Sub

bldOp e (oper,e') = oper e e'

factor = num ! var !
         lit '(' -# expr #- lit ')' !
         err "illegal factor"

term' e = mulOp # factor >-> bldOp e #> term' ! return e
term = factor #> term'

expr' e = addOp # term >-> bldOp e #> expr' ! return e
expr = term #> expr'

parens cond str = if cond then "(" ++ str ++ ")" else str

toString' :: Int -> Expr -> String
toString' prec (Add t u) = parens (prec>5) (toString' 5 t ++ "+" ++ toString' 5 u)
toString' prec (Sub t u) = parens (prec>5) (toString' 5 t ++ "-" ++ toString' 6 u)
toString' prec (Mul t u) = parens (prec>6) (toString' 6 t ++ "*" ++ toString' 6 u)
toString' prec (Div t u) = parens (prec>6) (toString' 6 t ++ "/" ++ toString' 7 u)
toString' prec (Exp t u) = parens (prec>6) (toString' 7 t ++ "^" ++ toString' 8 u)
toString' prec (Num n) = show n
toString' prec (Var v) = v

value :: Expr -> Dictionary.T String Integer -> Integer
value (Num n) _    = n
value (Add l r) d  = value l d + value r d
value (Sub l r) d  = value l d - value r d
value (Mul l r) d  = value l d * value r d
value (Exp l r) d  = value l d ^ value r d
value (Var v)   d  = fromMaybe (error "WARNING: ROUGE VARIABLE") (Dictionary.lookup v d)
value (Div l r) d  = case value r d of
  0  -> error "DIV ZERO ANOMALY DETECTED"
  _  -> value l d `div` value r d


instance Parse Expr where
    parse = expr
    toString = toString' 0
