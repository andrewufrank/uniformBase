-----------------------------------------------------------------------------
--Main.hs
-- Module      :  Main
-- Copyright   :  andrew u frank 2016
--
-- | test  the error modue
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
import Uniform.Strings
import TestingConvenience
import Uniform.Zero
import System.Exit
--import Data.Easy

programName = "error-0.0.7"

debug_main  =  True


main :: IO ()
main = do
    putIOwords  ["start  \n" ]
    putIOwords  [ "------------------ ", programName
--                , toText versionNum
            , " -------------------------"]
    r1 <- convenienceTest
    putIOwords["main", programName, "returning\n"
            , show' r1
            , "-------------------------\n\n"]
    let bs = [ r1 ]
    putIOwords ["convenienceTest  Main", showT bs]
    if (and bs) then exitSuccess else exitFailure



