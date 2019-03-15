{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv ((.:), (.=))
import qualified Data.Csv as CSV
import Data.List (sort)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Vector as V

-- zipcode,state,county_code,name,rate_area
-- 36749,AL,01001,Autauga,11

data ZipCode = ZipCode
  { zipcode :: String
  , zipState :: String
  , zipRateArea :: String
  }

instance CSV.FromNamedRecord ZipCode where
  parseNamedRecord r = ZipCode <$> r .: "zipcode"
                               <*> r .: "state"
                               <*> r .: "rate_area"

-- plan_id,state,metal_level,rate,rate_area
-- 74449NR9870320,GA,Silver,298.62,7

data Plan = Plan
  { planState :: String
  , metalLevel :: String
  , rate :: Float
  , planRateArea :: String
  }

instance CSV.FromNamedRecord Plan where
  parseNamedRecord r = Plan <$> r .: "state"
                            <*> r .: "metal_level"
                            <*> r .: "rate"
                            <*> r .: "rate_area"

-- a PlanRequest is one of the SLCSPs we need to look up by ZIP code
-- zipcode,rate
-- 64148,
data PlanRequest = PlanRequest
  { requestZIPCode :: String
  , requestRate :: String
  }
instance CSV.FromNamedRecord PlanRequest where
  parseNamedRecord r = PlanRequest <$> r .: "zipcode"
                                   <*> r .: "rate"
instance CSV.ToNamedRecord PlanRequest where
  toNamedRecord (PlanRequest z r) = CSV.namedRecord [
    "zipcode" .= z, "rate" .= r]
instance CSV.DefaultOrdered PlanRequest where
  headerOrder _ = CSV.header ["zipcode", "rate"]

type StateRateArea = (String, String)

zipsFromFile :: String -> IO [ZipCode]
zipsFromFile filename = do
  csvData <- BL.readFile filename
  case CSV.decodeByName csvData of
    Left err -> error ("Failed to parse file: " ++ show err)
    Right (_, v) -> return $ V.toList v

plansFromFile :: String -> IO [Plan]
plansFromFile filename = do
  csvData <- BL.readFile filename
  case CSV.decodeByName csvData of
    Left err -> error ("Failed to parse file: " ++ show err)
    Right (_, v) -> return $ V.toList v

requestsFromFile :: String -> IO [PlanRequest]
requestsFromFile filename = do
  csvData <- BL.readFile filename
  case CSV.decodeByName csvData of
    Left err -> error ("Failed to parse file: " ++ show err)
    Right (_, v) -> return $ V.toList v

type ZIPTable = Map String StateRateArea
rateAreasByZip :: [ZipCode] -> ZIPTable
rateAreasByZip zips = let
  makePair zip = (zipcode zip, (zipState zip, zipRateArea zip))
  pairs = map makePair zips
  in Map.fromList pairs

type RateTable = Map StateRateArea [Float]
silverPlansByRateArea :: [Plan] -> RateTable
silverPlansByRateArea plans = let
  isSilver plan = metalLevel plan == "Silver"
  silverPlans = filter isSilver plans
  makePair plan = ((planState plan, planRateArea plan), rate plan)
  pairs = map makePair silverPlans
  insertIntoMap m (newArea, newRate) = Map.insertWith (++) newArea [newRate] m
  in foldl insertIntoMap Map.empty pairs

secondLowestCost :: RateTable -> StateRateArea -> Maybe Float
secondLowestCost table rateArea
  | Map.notMember rateArea table = Nothing
  | length rates < 2 = Nothing
  | otherwise = Just (rates!!1)
  where rates = sort $ table!rateArea

secondLowestCostByZIP :: ZIPTable -> RateTable -> String -> Maybe Float
secondLowestCostByZIP zipTable rateTable zip
  | Map.notMember zip zipTable = Nothing
  | otherwise = secondLowestCost rateTable rateArea
  where rateArea = zipTable!zip

formattedSLCSP :: ZIPTable -> RateTable -> String -> String
formattedSLCSP zipTable rateTable zip =
  case (secondLowestCostByZIP zipTable rateTable zip) of
    Nothing -> ""
    Just rate -> show rate

lookupRequest :: ZIPTable -> RateTable -> PlanRequest -> PlanRequest
lookupRequest zipTable rateTable req = let
  rateStr = formattedSLCSP zipTable rateTable (requestZIPCode req)
  in req { requestRate = rateStr }

main :: IO ()
main = do
  plans <- plansFromFile "input/plans.csv"
  zips <- zipsFromFile "input/zips.csv"
  requests <- requestsFromFile "input/slcsp.csv"
  let zipTable = rateAreasByZip zips
  let rateTable = silverPlansByRateArea plans
  let finalPlans = map (lookupRequest zipTable rateTable) requests
  BL.putStrLn $ CSV.encodeDefaultOrderedByName $ finalPlans
