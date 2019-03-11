-----------------------------------------------------------------------------
--
-- Module      :   top tests for time
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

    {-# LANGUAGE
    MultiParamTypeClasses
    , TypeSynonymInstances
--    , FunctionalDependencies
    , FlexibleInstances
    , FlexibleContexts
    , ScopedTypeVariables
    , UndecidableInstances
    , OverloadedStrings
    , TypeFamilies

    #-}

module Main     where


import Uniform.Strings

import {-@ HTF_TESTS @-} Uniform.Time
import Uniform.Time as UT
import Test.Framework
import Data.Time
--import System.Time (calendarTimeToString)

main = do
    putIOwords ["HTF Time:\n "]
    r <- htfMainWithArgs ["--quiet"] htf_thisModulesTests
                -- htf_importedTests
    putIOwords ["HTF end LayoutTest.hs:\n posTest"]
    return r

timeTest ::   IO [Bool]
timeTest = do
    r <- runErr timeTest2
    v1 <- case r of
        Left msg -> do
                putIOwords ["errorTest returned Left :", msg]
                return [False]
        Right v -> return v
    return v1




timeTest2 ::   ErrIO [Bool]
-- examples how to use
timeTest2 = do
    t1 <- getCurrentTimeUTC
    putIOwords ["now",   showT t1]
    let     t2 = addSeconds 4.0 t1
            d4 = diffSeconds t2 t1
            x1 = d4 == 4.0
    putIOwords ["diff d4", showT d4, showT t2]

    let day1 = toYMD t1


    let     tomorrow = addSeconds (24 * 60 * 60) t1
            day2 = toYMD tomorrow
            oneDay = UT.diffDays  tomorrow t1
            x2 = oneDay == 1

    putIOwords ["today is", showT day1, "tomorrow", showT day2]


    return [x1, x2, True]


test_date1 =
  do
    let dateString = "26 Jan 2012 10:54 AM"
    let timeFromString = parseTimeOrError True defaultTimeLocale "%d %b %Y %l:%M %p" dateString :: UTCTime
    -- Format YYYY/MM/DD HH:MM
    print $ formatTime defaultTimeLocale "%Y/%m/%d %H:%M" timeFromString
    -- Format MM/DD/YYYY hh:MM AM/PM
    let res = formatTime defaultTimeLocale "%m/%d/%Y %I:%M %p" timeFromString
    assertEqual "01/26/2012 10:54 AM" res

test_date2 = do
    -- now for a string with single digit months and days:
    let dateString = "9-8-2012 10:54 AM"
    let timeFromString = parseTimeOrError True defaultTimeLocale "%-d-%-m-%Y %l:%M %p" dateString :: UTCTime
    -- Format YYYY/MM/DD HH:MM
    let res = formatTime defaultTimeLocale "%Y/%m/%d %H:%M" timeFromString
--    let res = formatTime defaultTimeLocale "%b %d, %Y" timeFromString
    assertEqual "2012/08/09 10:54" res

test_date3 =
    assertEqual "2019-01-07 00:00:00 UTC" (showT $ readDate2 "Jan 7, 2019")

test_date4 = do
    let nowT = readDate2 "Jan 7, 2019"
    now <- getCurrentTime
    let res = formatTime defaultTimeLocale "%b %-d, %Y" nowT
    assertEqual "Jan 7, 2019" res

--test_date5= do
--    r1 :: ErrOrVal Text <- runErr $ getDateAsText
--    let r2 = fromRightNote "werw" r1
--    assertEqual "Mon Mar 11 17:37:12 CET 2019" r2

test_date3a =
    assertEqual r3a (map showT $ map readDate3 t3a)

r3a = []
t3a = ["Jan 3, 2019", "Jan. 3, 2019", "March 5, 2019"
    , "jan 6, 2019", "February 7, 2011"
    , "1.1.2018", "9.10.20", "2018-11-27"]
