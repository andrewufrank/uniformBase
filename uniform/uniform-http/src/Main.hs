-----------------------------------------------------------------------------
--Main.hs
-- Module      :  Main
-- Copyright   :  andrew u frank 2016
--
-- | test  for fileio
-- copy first a dir with a set of files (produced in makeFilesA)
-- copy, check that correct files (hidden and not hidden)
--
--
-----------------------------------------------------------------------------
{-# LANGUAGE
    MultiParamTypeClasses
    , TypeSynonymInstances
--    , FunctionalDependencies
    , FlexibleInstances
--    , FlexibleContexts
--    , DeriveFunctor
    , ScopedTypeVariables
--    , UndecidableInstances
    , OverloadedStrings
    #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Main (main) where


--import qualified Data.Text as T (Text)
import Data.Strings
import System.Exit
import TestingFileIO

programName = "fileio-test-main-0.0.7"

debug_main  =  True


main :: IO ()
main = do
    putIOwordsT ["start  \n" ]
    putIOwordsT [ "------------------ ", programName
--                , toText versionNum
            , " -------------------------"]
    r1 <- fileioTest
    putIOwords["main", programName, "returning\n"
            , unwords' . map show' $ r1
            , "-------------------------\n\n"]
    let bs = r1
    putIOwords ["Test Main", show bs]
    if (and bs) then exitSuccess else exitFailure



