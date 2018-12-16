module Main where

import Program
import System.IO
import System.Directory

main :: IO ()
main = inputChecker [] >>= (\(prog:vars) -> (putStrLn . (++) "\nResults: " . show . Program.exec (fromString prog) . read . head) vars)

inputChecker :: [String] -> IO [String]
inputChecker (prog:vars) = doesFileExist prog >>= (\exists -> if exists then readFile prog >>= (\cont -> return (cont:vars)) else return (prog:vars))
inputChecker  something  = mapM ((>> getLine) . putStrLn) ["Enter filepath or program", "Enter variables, e.g. [v1,...,vN]"] >>= inputChecker 