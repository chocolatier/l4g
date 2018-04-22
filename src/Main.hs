module Main where

import L4GParser
import System.Environment
import Text.Parsec

main :: IO ()
main = do
    args <- getArgs
    let sorts = args!!0
    sortFile <- readFile sorts 
    putStrLn $ show $ parse sortsParser "" sortFile