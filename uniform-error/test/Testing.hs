-----------------------------------------------------------------------------
--
-- Module      :   top tests for error 
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

    {-# LANGUAGE
    MultiParamTypeClasses
    , TypeSynonymInstances
--    , FunctionalDependencies
    , FlexibleInstances
    , FlexibleContexts
    , ScopedTypeVariables
--    , UndecidableInstances
    , OverloadedStrings
    , TypeFamilies

    #-}

module Main     where

import {-@ HTF_TESTS @-} Uniform.Error_test
import Test.Framework ( htfMainWithArgs, makeTestSuite, TestSuite )
    ( errorTest, htf_Uniform_Error_test_thisModulesTests  )
import Uniform.Strings ( putIOwords, showT )

main :: IO ()
main = do
    putIOwords ["HTF errorTest.hs:\n uniform-error test"]
    r <- htfMainWithArgs ["--quiet"] htf_importedTests
    putIOwords ["HTF end errorTest.hs:\n posTest", showT r]
    r2 <- errorTest
    putIOwords [" error test end\n  ", showT r2]
    return r




