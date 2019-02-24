module Main where

import           Lib

main :: IO ()
main = runProgramWrapped "~/side/u/u-test.sh" >>= print
