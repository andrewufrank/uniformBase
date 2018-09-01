-----------------------------------------------------------------------------
--
-- Module      :  read CSV files
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}

module Uniform.Convenience.ReadCSV -- (closedMain)
    where

import           Test.Framework
--import Uniform.Strings
--import Uniform.Error
import Text.CSV
import Text.Parsec.Error
import Uniform.FileIO

csvExtension = Extension "csv" :: Extension

-- Parses a CSV file
readCSV :: Path Abs File  -> ErrIO CSV
readCSV fileName = do
--  let fileName = "input.csv"
  let fileName1 = addExtension csvExtension fileName  :: Path Abs File
  putIOwords ["readCSV", "filename", s2t $ toShortFilePath fileName1]
  input <- callIO $ readFile (toShortFilePath fileName1)
  putIOwords ["readCSV", "input", s2t $ input]
  let csv = parseCSV (toShortFilePath fileName1) input
  -- filename is used only for error messages...
  case csv of
    Left e -> throwErrorT [s2t . show $ e]
    Right f -> return f

handleError csv = putStrLn "not a CSV"

--toInt :: String -> Int
--toInt = read

-- show produces the "xx"
test_1 = do
    r <- runErr $ readCSV (makeAbsFile "/home/test/input")

    assertEqual r  res

res = Right
  [["name", "age"], ["Alex", "22"], ["Anish", "22"], ["Becca", "23"],
   ["Jasdev", "22"], ["John", "21"], ["Jonathon", "21"],
   ["Kelvin", "22"], ["Marisa", "19"], ["Shiv", "22"],
   ["Vinay", "22"], [""]]


--doWork csv = (print.findOldest.tail) (filter (\x -> length x == 2) csv)
--
---- Finds oldest person.
--findOldest :: [Record] -> Record
--findOldest [] = []
--findOldest items = foldl1 (\a x -> if age x > age a then x else a) items
--
--age [a,b] = toInt b


