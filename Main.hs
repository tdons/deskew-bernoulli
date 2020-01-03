#!/usr/bin/env stack
{- stack runghc --resolver lts-6.0 -}

{- Quick and dirty test program :-) -}

import Deskew

chr2bit '1' = One
chr2bit '0' = Zero

bit2chr One = '1'
bit2chr Zero = '0'

p s = show $ (fromIntegral . length . filter (== '1') $ s) / (fromIntegral . length $ s)

removeBias = (map bit2chr) . deskew . (map chr2bit) . (filter (/= '\n'))

main :: IO ()
main = do
  input <- getLine
  let deskewed = removeBias input
  putStrLn $ " input: (p = " ++ (p input) ++ ")"
  putStrLn $ "output: (p = " ++ (p deskewed) ++ ")\n" ++ deskewed
