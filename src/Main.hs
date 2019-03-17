module Main where

import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Csv as CSV

import SLCSP

main :: IO ()
main = do
  plans <- plansFromFile "input/plans.csv"
  zips <- zipsFromFile "input/zips.csv"
  requests <- requestsFromFile "input/slcsp.csv"
  let zipTable = rateAreasByZip zips
  let rateTable = silverPlansByRateArea plans
  let finalPlans = map (lookupRequest zipTable rateTable) requests
  Char8.putStrLn $ CSV.encodeDefaultOrderedByName $ finalPlans
