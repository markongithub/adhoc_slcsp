module Main where

import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Csv as CSV

import System.Console.CmdArgs
import SLCSP

data Options = Options {
    planFile :: String
  , zipFile :: String
  , requestFile :: String
} deriving (Data, Typeable)

options = Options { planFile = "input/plans.csv"
                  , zipFile = "input/zips.csv"
                  , requestFile = "input/slcsp.csv"
                  }

main :: IO ()
main = do
  opts <- cmdArgs options
  plans <- plansFromFile (planFile opts)
  zips <- zipsFromFile (zipFile opts)
  requests <- requestsFromFile (requestFile opts)
  let zipTable = rateAreasByZip zips
  let rateTable = silverPlansByRateArea plans
  let finalPlans = map (lookupRequest zipTable rateTable) requests
  Char8.putStrLn $ CSV.encodeDefaultOrderedByName $ finalPlans
