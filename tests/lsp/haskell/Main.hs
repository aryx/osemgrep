module Main where

import Lib (add, ratio)

main :: IO ()
main = do
  let a = 10
  let b = 20
  -- Lib.add is defined in Lib.hs.
  -- Only HLS knows add a b has type Int.
  let sum = add a b
  -- Lib.ratio returns Double, so this should NOT match ($X :: Int).
  let r = ratio a b
  putStrLn ("sum=" ++ show sum ++ " ratio=" ++ show r)
