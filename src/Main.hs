module Main where

import JsonParser
import Custom
import AST


parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
  input <- readFile fileName
  return (snd <$> runParser parser input)

main :: IO ()
main = do
  putStrLn "hello world"
