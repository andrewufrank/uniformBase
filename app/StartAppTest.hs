-----------------------------------------------------------------------------
--
-- Module      : the main for calling an application for testing uniform 
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}


module Main  where      -- must have Main (main) or Main where

import           Uniform.Convenience.StartApp
import           Uniform.Filenames           --   ( makeExtension )
-- import           Data.Semigroup                 ( (<>) )
import           Options.Applicative.Builder
import           Options.Applicative
import           Lib.ProcTxt
import           Lib.ProcPandocDatei

programName, progTitle :: Text
programName = "Test Uniform package" :: Text
progTitle =
  "Test uniform package build" :: Text

-- to run add in .ghci -- tests/Testing.hs

main :: IO ()
main = do
  startProg
    programName
    progTitle
    (someFunc
      (unlinesT
        [ "test the uniform package and its building"
        ]
      )
      "no arguments"
    )
  return ()


someFunc :: IO ()
someFunc = putStrLn "someFunc"
