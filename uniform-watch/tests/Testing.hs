-----------------------------------------------------------------------------
--
-- Module      :   top tests for layout
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

    {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,
  FlexibleContexts, ScopedTypeVariables, OverloadedStrings,
  TypeFamilies #-}

module Main     where



import Test.Framework
import {-@ HTF_TESTS @-} Uniform.Watch_test
import Uniform.TwitchMinimal 
import Uniform.Strings
import Uniform.Watch_test
import Uniform.Error

main = do
    putIOwords ["HTF watchTest.hs:\n uniform-watch test"]
    r <- htfMainWithArgs ["--quiet"] htf_importedTests
    putIOwords ["HTF end examp;eTest.hs:\n", showT r]

    return r

main3 = mainTwitchMinimal 

main4 = runErr $ mainWatch [testWatch]
main5 = runErr $ mainWatch3
main6 = runErr $ mainWatch [testWatch, testWatch2]



