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

-- import           Uniform.Convenience.StartApp
-- import           Uniform.Filenames           --   ( makeExtension )
-- -- import           Data.Semigroup                 ( (<>) )
-- import           Options.Applicative.Builder
-- import           Options.Applicative
-- import           Lib.ProcTxt
-- import           Lib.ProcPandocDatei
import UniformBase

programName  :: Text
programName = "Test Uniform package" :: Text

-- to run add in .ghci -- tests/Testing.hs

main :: IO ()
main = do
  startProg
    programName
    (someFunc
      (unlinesT
        [ "test the uniform package and its building"
        ]
      )
      "no arguments"
    )
  return ()


someFunc :: Text -> Text -> ErrIO ()
-- ^ the function must have two text arguments
someFunc a b = putIOwords ["someFunc", a, b]
