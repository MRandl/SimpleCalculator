module Lib
    ( someFunc
    ) where

import Text.ParserCombinators.ReadP ( readP_to_S )
import Parser ( inputParser )
import Data.Char ( isSpace )
import ShuntingYard ( evaluateTokenList )

someFunc :: IO ()
someFunc = do
  putStrLn ""
  i <- getLine 
  case evaluateTokenList $ fst $ head $ readP_to_S inputParser $ filter (not . isSpace) i of
    Nothing -> putStrLn "Could not compute result."
    Just re -> putStrLn $ "Computed result : " ++ show re
  someFunc