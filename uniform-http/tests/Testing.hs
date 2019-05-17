-----------------------------------------------------------------------------
--
-- Module      :   top tests for layout
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

module Main where

import Uniform.Strings
import           Test.Framework
-- import {-@ HTF_TESTS @-} Uniform.HttpCall_test
import {-@ HTF_TESTS @-} Uniform.HttpURI_test

localhostTextFile = "http://www.gerastree.at/testaf1" :: Text
-- parseRequest_  does not throw useful exception
-- for the conduit based http

main =  do
    putStrLn "Lit Text Test.hs:\n"
--    r <- htfMainWithArgs ["--colors=True", "--fail-fast"] htf_importedTests
    r <- htfMain htf_importedTests
    return ()


