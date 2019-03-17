module Main where

import qualified Data.Map as Map
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)

import SLCSP

testZipCodes =  [ ZipCode "00000" "NJ" "1"
                , ZipCode "11111" "VA" "2"
                , ZipCode "22222" "NH" "3"
                ]
zipTable = rateAreasByZip testZipCodes

testPlans = [ Plan "NJ" "Double Platinum" 99.99  "1"
            , Plan "NJ" "Silver"          191.18 "1"
            , Plan "NJ" "Silver"          200.00 "1"
            , Plan "VA" "Silver"          200.00 "2"
            ]
rateTable = silverPlansByRateArea testPlans

equalityTest description expected actual =
  testCase description $ assertEqual description expected actual

testOnlyOneSilverPlan = equalityTest "testOnlyOneSilverPlan" ""
                          $ formattedSLCSP zipTable rateTable "11111"

testNoRateAreaForZIP = equalityTest "testNoRateAreaForZIP" ""
                         $ formattedSLCSP zipTable rateTable "33333"

testNoPlansForRateArea = equalityTest "testNoPlansForRateArea" ""
                          $ formattedSLCSP zipTable rateTable "22222"

testCorrectSLCSP = equalityTest "testCorrectSLCSP" "200.00"
                     $ formattedSLCSP zipTable rateTable "00000"

main = defaultMain [ testOnlyOneSilverPlan
                   , testNoRateAreaForZIP
                   , testNoPlansForRateArea
                   , testCorrectSLCSP
                   ]

