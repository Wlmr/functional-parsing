module Main where

import Program
import System.IO
import System.Directory

main :: IO ()
main = inputChecker [] >>= (\(program:variables) -> (putStrLn . (++) "\nResults: " . show . Program.exec (fromString program) . read . head) variables)

inputChecker :: [String] -> IO [String]
inputChecker (prog:vars) = doesFileExist prog >>= (\exists -> if exists then readFile prog >>= (\content -> return (content:vars)) else return (prog:vars))
inputChecker  something  = sequence [putStrLn "Enter filepath or program: " >> getLine, putStrLn "\nEnter variables, e.g. [v1,...,vN]" >> getLine] >>= inputChecker 
                            



