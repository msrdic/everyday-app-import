{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ()
import qualified Data.ByteString.Lazy as BL
import Data.Csv ( (.:), decodeByName, FromNamedRecord(..) )
import Data.Text (pack, unpack)
import qualified Data.Vector as V
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)

import EverydayAppClient (Status, YYMMDD, HabitId, mark, skip, unmark)

data DateEntry = DateEntry
    { date   :: !String
    , water :: !String
    , mobility :: !String
    , pills_1 :: !String
    , pills_2 :: !String
    , pills_3 :: !String
    , floss :: !String
    , meditate :: !String
    }

instance FromNamedRecord DateEntry where
    parseNamedRecord r = DateEntry <$> r .: "date"
                                   <*> r .: "water"
                                   <*> r .: "mobility"
                                   <*> r .: "pills_1"
                                   <*> r .: "pills_2"
                                   <*> r .: "pills_3"
                                   <*> r .: "floss"
                                   <*> r .: "meditate"

-- 1: mark
-- 0: unmark
-- -: skip

extractDateParts :: String -> (String, String, String)
extractDateParts date = let (m, r1) = span (/= '/') date
                            (d, r2) = span (/= '/') (tail r1)
                            y = tail r2
                        in (d, m, y)

transformDate :: String -> YYMMDD
transformDate d = let yyyy = "20" ++ year -- all habits are after 2000...
                      mm = if length month == 1 then "0" ++ month else month
                      dd = if length day == 1 then "0" ++ day else day
                      (day, month, year) = extractDateParts d
                  in pack (yyyy ++ "-" ++ mm ++ "-" ++ dd)

data Action = Mark | Skip | Unmark

performHabitAction :: HabitId -> YYMMDD -> Action -> IO Status
performHabitAction habitId date Mark = mark date habitId
performHabitAction habitId date Skip = skip date habitId
performHabitAction habitId date Unmark = unmark date habitId

performHabit :: HabitId -> YYMMDD -> String -> IO Status
performHabit hid d val | val == "1" = performHabitAction hid d Mark
                       | val == "0" = performHabitAction hid d Unmark
                       | otherwise  = performHabitAction hid d Skip

pills :: String -> String -> String -> String
pills "1" _ _ = "1"
pills _ "1" _ = "1"
pills _ _ "1" = "1"
pills "0" "0" "0" = "0"
pills _ _ _ = "-"

-- very specific, do not use on your data!
main :: IO ()
main = do
    csvData <- BL.readFile "./data/habits.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ (V.reverse v) $ \ de -> do
          let d = transformDate (date de)
              wat = water de
              mob = mobility de
              p1 = pills_1 de
              p2 = pills_2 de
              p3 = pills_3 de
              fl = floss de
              med = meditate de

          -- water: 362803
          s1 <- performHabit 362803 d wat
          -- pills: 363050
          s2 <- performHabit 363050 d (pills p1 p2 p3)
          -- meditate: 362793
          s3 <- performHabit 362793 d med
          -- mobility: 577202
          s4 <- performHabit 577202 d mob
          -- floss: 577209
          s5 <- performHabit 577209 d fl

          putStrLn $ unpack d ++ ": " ++ show s1 ++ "," ++ show s2 ++ "," ++ show s3 ++ "," ++ show s4 ++ "," ++ show s5
          
          liftIO $ threadDelay (500 * 000)