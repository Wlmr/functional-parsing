module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Data.List
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving Show

statements :: Parser T
statements = iter Statement.parse >-> buildStatements
buildStatements :: [Statement.T] -> T
buildStatements = Program

instance Parse T where
    parse = iter Statement.parse >-> Program
    toString (Program stmts) = intercalate "\n" $ map toString stmts

exec :: T -> [Integer] -> [Integer]
exec (Program stmts) = Statement.exec stmts Dictionary.empty

-- toString' :: T -> String
-- toString' (Program stmts) = concatMap Statement.toString stmts
