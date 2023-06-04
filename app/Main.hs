module Main where

import Numeric ( showFFloat )
import Lib ( p, sequence1, sequence2, sequence3, sequence4 )

main :: IO ()
main = do
  putStrLn $ "p         = " ++ show p ++ "\n"
  putStrLn $ "sequence1 = " ++ showRounded 16 sequence1 ++ "\n"
  putStrLn $ "sequence2 = " ++ showRounded 6 sequence2 ++ "\n"
  putStrLn $ "sequence3 = " ++ showRounded 16 sequence3 ++ "\n"
  putStrLn $ "sequence4 = " ++ showRounded 6 sequence4


showRounded :: Int -> [Double] -> String
showRounded n lst = 
  let
    ss s = showFFloat (Just n) s ""
  in
    filter (/= '\"') . show $ map ss lst