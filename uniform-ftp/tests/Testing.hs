-----------------------------------------------------------------------------
--
-- Module      :   top tests for layout
-- import with  {-@ HTF_TESTS @-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances,
             --    , FunctionalDependencies
             FlexibleInstances, FlexibleContexts, ScopedTypeVariables,
             --    , UndecidableInstances
             OverloadedStrings, TypeFamilies #-}

module Main where

import           Test.Framework
import   {-@ HTF_TESTS @-}         Uniform.Ftp_test
import           Uniform.Strings hiding ((</>), (<.>))
import           Uniform.Error
import Uniform.Ftp
import Control.Monad.Trans.State 

main :: IO ()
main = do
  putIOwords ["HTF exampleTest.hs:\n uniform-ftp test"]
  r <- htfMainWithArgs ["--quiet"] htf_importedTests
  putIOwords ["HTF end exampleTest.hs:\n", showT r]
  return r

main2 :: IO ()
main2      -- just a simple bake for test
  = do
    putStrLn "main2"
    -- push2
    runErrorVoid $ do 
      (a,s)  <- runStateT mainStateIOc ftp0
      return () 






