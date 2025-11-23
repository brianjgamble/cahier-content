module Main where

import Test.DocTest (doctest)

main :: IO ()
main =
  doctest
    [ "--fast"
    , "-XOverloadedStrings"
    , "src-internal/Cahier/Content/Util.hs"
    ]
