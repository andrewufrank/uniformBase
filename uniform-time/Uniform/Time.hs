-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Time
--
-- | a minimal set of time operations
-- at the moment only a wrapper to time
-- examples in TestingTime.hs
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE
     MultiParamTypeClasses
    , FlexibleInstances
    , FlexibleContexts
    , UndecidableInstances
    , ScopedTypeVariables
    , DeriveDataTypeable   -- needed
    , TypeSynonymInstances
--    , DoAndIfThenElse
--    , TypeFamilies
--    , ConstraintKinds
--    , BangPatterns
    , OverloadedStrings
             #-}


module Uniform.Time (
        module Uniform.Time
        , module Uniform.Error   -- or at least ErrIO
        -- , module Uniform.Strings
        , UTCTime , EpochTime
--    , htf_thisModulesTests
        )  where

import Test.Framework

import Data.Time as T
import Uniform.Error
-- import Uniform.Strings
import Data.Convertible
import System.Posix.Types (EpochTime)
--import System.Time (getClockTime, toCalendarTime, calendarTimeToString)

--class Times a where
--    type TimeUTC  a
--    type YMD a
--
--    getCurrentTimeUTC :: ErrIO a
--    addSeconds :: Double -> a ->  a
--    diffSeconds :: a -> a -> T.NominalDiffTime
--
--    toYMD :: a -> YMD a
--    diffDays :: a -> a -> Integer
--
--instance Times UTCTime where
--    type TimeUTC UTCTime =  T.UTCTime
--    type YMD UTCTime = (Integer, Int, Int)

instance CharChains2 UTCTime Text where
    show' = s2t . show
instance CharChains2 T.NominalDiffTime Text where
    show' = s2t . show
instance CharChains2 (Integer, Int, Int) Text where
    show' = s2t . show


getCurrentTimeUTC :: ErrIO UTCTime
addSeconds :: Double -> UTCTime -> UTCTime
diffSeconds :: UTCTime -> UTCTime  -> T.NominalDiffTime

getCurrentTimeUTC = liftIO $ T.getCurrentTime
addSeconds s t = T.addUTCTime (realToFrac s) t
diffSeconds a b =  T.diffUTCTime a b

toYMD = T.toGregorian . T.utctDay
diffDays a b = T.diffDays (T.utctDay a) (T.utctDay b)

epochTime2UTCTime :: EpochTime -> UTCTime
epochTime2UTCTime = convert

getDateAsText :: ErrIO Text
getDateAsText = callIO $ do
            now <- getCurrentTime
            let res = formatTime defaultTimeLocale "%b %-d, %Y" now
            return . s2t $ res
--                        t <-  getClockTime
--                        tc <- toCalendarTime t
--                        return (s2t $ calendarTimeToString tc)

readDate2 :: Text ->  UTCTime
-- ^ read data in the Jan 7, 2019 format (no . after month)
readDate2 datestring = parseTimeOrError True defaultTimeLocale
            "%b %-d, %Y" (t2s datestring) :: UTCTime

readDate3 :: Text ->   UTCTime
-- ^ read data in the Jan 7, 2019 format (no . after month)
readDate3 datestring  =

    case shortMonth of
        Just t -> t
        Nothing -> case longMonth of
            Just t2 -> t2
            Nothing -> case monthPoint of
                Just t3 -> t3
                Nothing -> case germanNumeralShort of
                  Just t3 -> t3
                  Nothing -> case germanNumeral of
                    Just t3 -> t3
                    Nothing -> case isoformat of
                      Just t4 -> t4
                      Nothing -> errorT   ["readDate3", datestring, "is not parsed"]

    where
        shortMonth = parseTimeM True defaultTimeLocale
            "%b %-d, %Y" (t2s datestring) :: Maybe UTCTime
        longMonth = parseTimeM True defaultTimeLocale
            "%B %-d, %Y" (t2s datestring) :: Maybe UTCTime
        monthPoint = parseTimeM True defaultTimeLocale
            "%b. %-d, %Y" (t2s datestring) :: Maybe UTCTime
        germanNumeral = parseTimeM True defaultTimeLocale
            "%-d.%-m.%Y" (t2s datestring) :: Maybe UTCTime
        germanNumeralShort = parseTimeM True defaultTimeLocale
            "%-d.%-m.%y" (t2s datestring) :: Maybe UTCTime
        isoformat = parseTimeM True defaultTimeLocale
            "%Y-%m-%d" (t2s datestring) :: Maybe UTCTime
