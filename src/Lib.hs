module Lib ( libMain ) where

import Data.Char    ( isSpace )
import Data.Maybe   ( listToMaybe )
import Text.ParserCombinators.ReadP ( readP_to_S )
import Parser       ( inputParser )
import ShuntingYard ( evaluateTokenList )

libMain :: IO ()
libMain = do
  putStrLn ""
  i <- getLine 
  let parsed = readP_to_S inputParser $ filter (not . isSpace) i in 
    case evaluateTokenList . fst =<< listToMaybe parsed of
    Nothing -> putStrLn "Could not compute result."
    Just re -> putStrLn $ "Computed result : " ++ show re
  libMain