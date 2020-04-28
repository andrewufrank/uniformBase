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
--    , UndecidableInstances
    , OverloadedStrings
    , TypeFamilies

    #-}

module Main     where



import Test.Framework
import Uniform.Example 
import Uniform.Strings
-- import Uniform.Error 
import  {-@ HTF_TESTS @-} Uniform.Example_test 

main = do
    putIOwords ["HTF exampleTest.hs:\n uniform-example test"]
    r <- htfMainWithArgs ["--quiet"] htf_importedTests
    putIOwords ["HTF end examp;eTest.hs:\n", showT r]
    
    return r




