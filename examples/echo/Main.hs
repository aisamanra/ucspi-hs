module Main where

import Network.UCSPI as UCSPI

main :: IO ()
main = UCSPI.ucspiServer $ \ _ -> getLine >>= putStrLn
