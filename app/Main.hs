module Main where

import Program
import System.Environment
import System.IO
import System.Directory
import Control.Monad

main :: IO ()
main = inputChecker [""] >>= (\(program:variables) -> print $ "Results: " ++ (show . Program.exec (fromString program)) (read (head variables)))

inputChecker :: [String] -> IO [String]
inputChecker (prog:vars) = doesFileExist prog >>= (\exists -> if exists then readFile prog >>= (\content -> return (content : vars)) else return (prog:vars))
inputChecker whatever    = sequence [putStr "\nEnter filename or program\n" >> getLine, putStr "Enter variables [v1,v2,...,vN]\n" >> getLine] >>= inputChecker 
                            



