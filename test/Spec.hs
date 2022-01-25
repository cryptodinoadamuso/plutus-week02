module Main
    ( main
    ) where

import qualified Spec.Homework2
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "homework 1 tests"
    [ Spec.Homework2.tests
    ]