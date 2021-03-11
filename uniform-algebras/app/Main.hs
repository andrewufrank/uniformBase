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
--    , OverloadedStrings
    #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Main (main) where


--import qualified Data.Text as T (Text)
--import Data.Strings
-- import TestingAlgebras
-- import Uniform.Zero
-- import System.Exit
--import Data.Easy

programName = "algebras"

debug_main  =  True

putIOwords = putStrLn . unwords

main :: IO ()
main = do
    putIOwords ["start  \n" ]
    putIOwords [ "------------------ ", programName
--                , toText versionNum
            , " -------------------------"]
    r1 <- algebrasTest
    putIOwords["main", programName, "returning\n"
            , show r1
            , "-------------------------\n\n"]
    let bs = [ r1 ]
    putIOwords ["Algebra Test Main", show bs]
    if (and bs) then exitSuccess else exitFailure



