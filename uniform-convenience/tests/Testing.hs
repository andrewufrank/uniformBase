-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

import           Test.Framework
--import {-@ HTF_TESTS @-} ClosedClass
--import {-@ HTF_TESTS @-} OpenClass
--import {-@ HTF_TESTS @-} Lib.Tutorial1_test
--import {-@ HTF_TESTS @-} Lib.BlogExample_test
--import {-@ HTF_TESTS @-} Lib.ParseJsonCoreNLP_test
--import {-@ HTF_TESTS @-} Lib.CorefOnlyEdited_test
--import {-@ HTF_TESTS @-} Lib.CorefOnly_test
--import {-@ HTF_TESTS @-} Lib.Doc2ToDoc0_test
--import {-@ HTF_TESTS @-} Lib.ProduceNLPtriples2_test
import {-@ HTF_TESTS @-} Uniform.Test.TestHarness_test
-- main =  do  -- the local tests only
--     putStrLn "HTF ExampleTest.hs:\n"
--     r <- htfMain htf_thisModulesTests
--     putStrLn ("HTF end ExampleTesting.hs test:\n" ++ show r)
--     return ()

main =  do  -- with tests in other modules
    putStrLn "HTF ExampleTest.hs:\n"
    p <- htfMain htf_importedTests
    putStrLn ("HTF end StringConversion.hs test:\n" ++ show p)
    return ()

--myReverse :: [a] -> [a]
--myReverse []     = []
--myReverse [x]    = [x]
--myReverse (x:xs) = myReverse xs ++ [x]
--
---- start function name with test for a tests with given results
--test_nonEmpty = do assertEqual [1] (myReverse [1])
--                   assertEqual [3,2,1] (myReverse [1,2,3])
--
--test_empty = assertEqual ([] :: [Int]) (myReverse [])
--
---- start function name with prop for testing with random generated values
--prop_reverse :: [Int] -> Bool
--prop_reverse xs = xs == myReverse (myReverse xs)
--
---- a test which failed can be repeated
--prop_reverseReplay =
--  withQCArgs (\a -> a { replay = read
--    "Just (TFGenR 000034A28CA0BA65000000003B9ACA00000000000000E1F70000000000\
--        \000000 0 127 7 0,6)" })
--  prop_reverse
