module Tests where

import ElmTestBDDStyle exposing (..)

tests : Test
tests =
  describe "A Test Suite"
    [ it "adds two numbers" <|
        expect (3 + 7) toBe 10

    , it "passes for non-sense stuff" <|
        expect True toBe True
    ]
