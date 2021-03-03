module Main (main) where

import System.Process (callProcess)

main :: IO ()
main = callProcess "icd-test/hw3.sh" []
