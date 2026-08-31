module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import ElabTests (elabTests)

main :: IO ()
main = defaultMain =<< testGroup "ibis-lang" <$> sequence [elabTests]
