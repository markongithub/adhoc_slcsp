module Main where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)

import SLCSP

oneTest = testCase "please pass" $ assertEqual "whatever" 1 1

main = defaultMain [oneTest]

