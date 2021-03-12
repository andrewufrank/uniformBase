-----------------------------------------------------------------------------
--
-- Module      :  Uniform.TestHarness
--
-- | functions to deal wtih tests which
-- store data on disk
-- naming test{Var}NFile{IO}
-- N gives the number of input files 0..2
-- IO is present when the function is -> ErrIO x

-- attention: the test result throws an exception HUnit.NN (caused by assertBool)



-----------------------------------------------------------Utils.hs------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns          #-}
--{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is depreceiated but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.TestHarness (module Uniform.Test.TestHarness
    , module Uniform.Error
    , module Test.Framework
    , ShowTestHarness (..)
    , FilePath
    , getLitTextTestDir3
    , module Uniform.Test.Utils    -- for instances

        )  where

import           Safe
import           Test.Framework
import Uniform.FileIO
import Uniform.Error  hiding ((</>), (<.>)) -- to allow export
import Uniform.Test.Utils

testvardebug = True -- False

-- cases with no IO
testVar0File :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
            => Text -> a -> FilePath -> (a->  b) -> IO ()
testVar0File progName  a resfile op = do
        when testvardebug $ putIOwords ["testVar0File read text "]
        r <- runErr $  sub progName  a resfile op
        assertEqual (Right True) r
    where
        sub progName a resfile op =    do
            testDataDir <- getLitTextTestDir3 progName
            let t1 = op a
            checkResult testvardebug testDataDir resfile t1

testVar1File :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b
                , Zeros c, Eq c, Show c, Read c, ShowTestHarness c)
            => Text -> a -> FilePath -> FilePath -> (a->  b -> c ) -> IO ()
testVar1File progName  a startfile resfile op = do
        when testvardebug $ putIOwords ["testVar0File read text "]
        r <- runErr $  sub progName  a startfile resfile op
        assertEqual (Right True) r
    where
        sub progName a startfile resfile op =    do
            testDataDir <- getLitTextTestDir3 progName
            f0 <- readStartFile3 testvardebug testDataDir startfile
            let t1 = op a (readTestH2 startfile f0)
            checkResult testvardebug testDataDir resfile t1


testVar2File :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b
                , Zeros c, Eq c, Show c, Read c, ShowTestHarness c
                , Zeros d, Read d, Show d, Eq d, ShowTestHarness d)
            => Text -> a -> FilePath -> FilePath -> FilePath
                    -> (a->  b -> c -> d ) -> IO ()
testVar2File progName  a startfile secfile resfile op = do
        when testvardebug $ putIOwords ["testVar0File read text "]
        r <- runErr $  sub progName  a startfile secfile resfile op
        assertEqual (Right True) r
    where
        sub progName a startfile secfile resfile op =    do
            testDataDir <- getLitTextTestDir3 progName
            f0 <- readStartFile3 testvardebug testDataDir startfile
            f2 <- readStartFile3 testvardebug testDataDir secfile
            let t1 = op a (readTestH2 startfile f0) (readTestH2 secfile f2)
            checkResult testvardebug testDataDir resfile t1

test1File :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b
                , Zeros c, Eq c, Show c, Read c, ShowTestHarness c)
            => Text ->  FilePath -> FilePath -> (b -> c ) -> IO ()
test1File progName  startfile resfile op = do
        when testvardebug $ putIOwords ["testVar0File read text "]
        r <- runErr $  sub progName   startfile resfile op
        assertEqual (Right True) r
    where
        sub progName  startfile resfile op =    do
            testDataDir <- getLitTextTestDir3 progName
            f0 <- readStartFile3 testvardebug testDataDir startfile
            let t1 = op  (readTestH2 startfile f0)
            checkResult testvardebug testDataDir resfile t1


test2File :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b
                , Zeros c, Eq c, Show c, Read c, ShowTestHarness c
                , Zeros d, Read d, Show d, Eq d, ShowTestHarness d)
            => Text ->  FilePath -> FilePath -> FilePath
                    -> ( b -> c -> d ) -> IO ()
test2File progName  startfile secfile resfile op = do
        when testvardebug $ putIOwords ["testVar0File read text "]
        r <- runErr $  sub progName  startfile secfile resfile op
        assertEqual (Right True) r
    where
        sub progName  startfile secfile resfile op =    do
            testDataDir <- getLitTextTestDir3 progName
            f0 <- readStartFile3 testvardebug testDataDir startfile
            f2 <- readStartFile3 testvardebug testDataDir secfile
            let t1 = op  (readTestH2 startfile f0) (readTestH2 secfile f2)
            checkResult testvardebug testDataDir resfile t1

------ cases with IO

testVar0FileIO :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
            => Text -> a -> FilePath -> (a-> ErrIO b) -> IO ()
testVar0FileIO progName  a resfile op = do
        when testvardebug $ putIOwords ["testVar0FileIO read text "]
        r <- runErr $  sub progName  a resfile op
        assertEqual (Right True) r
    where
        sub progName a resfile op =    do
            testDataDir <- getLitTextTestDir3 progName
            t1 <- op a
            checkResult testvardebug testDataDir resfile t1

testVar1FileIO :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b
                , Zeros c, Eq c, Show c, Read c, ShowTestHarness c)
            => Text -> a -> FilePath -> FilePath -> (a->  b -> ErrIO c) -> IO ()
testVar1FileIO progName  a startfile resfile op = do
        when testvardebug $ putIOwords ["testVar0FileIO read text "]
        r <- runErr $  sub progName  a startfile resfile op
        assertEqual (Right True) r
    where
        sub progName a startfile resfile op =    do
            testDataDir <- getLitTextTestDir3 progName
            f0 <- readStartFile3 testvardebug testDataDir startfile
            t1 <- op a (readTestH2 startfile f0)
            checkResult testvardebug testDataDir resfile t1


testVar2FileIO :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b
                , Zeros c, Eq c, Show c, Read c, ShowTestHarness c
                , Zeros d, Read d, Show d, Eq d, ShowTestHarness d)
            => Text -> a -> FilePath -> FilePath -> FilePath
                    -> (a->  b -> c -> ErrIO d) -> IO ()
testVar2FileIO progName  a startfile secfile resfile op = do
        when testvardebug $ putIOwords ["testVar0FileIO read text "]
        r <- runErr $  sub progName  a startfile secfile resfile op
        assertEqual (Right True) r
    where
        sub progName a startfile secfile resfile op =    do
            testDataDir <- getLitTextTestDir3 progName
            f0 <- readStartFile3 testvardebug testDataDir startfile
            f2 <- readStartFile3 testvardebug testDataDir secfile
            t1 <- op a (readTestH2 startfile f0) (readTestH2 secfile f2)
            checkResult testvardebug testDataDir resfile t1

test1FileIO :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b
                , Zeros c, Eq c, Show c, Read c, ShowTestHarness c)
            => Text ->  FilePath -> FilePath -> (b -> ErrIO c) -> IO ()
test1FileIO progName  startfile resfile op = do
        when testvardebug $ putIOwords ["testVar0FileIO read text "]
        r <- runErr $  sub progName   startfile resfile op
        assertEqual (Right True) r
    where
        sub progName  startfile resfile op =    do
            testDataDir <- getLitTextTestDir3 progName
            f0 <- readStartFile3 testvardebug testDataDir startfile
            t1 <- op  (readTestH2 startfile f0)
            checkResult testvardebug testDataDir resfile t1


test2FileIO :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b
                , Zeros c, Eq c, Show c, Read c, ShowTestHarness c
                , Zeros d, Read d, Show d, Eq d, ShowTestHarness d)
            => Text ->  FilePath -> FilePath -> FilePath
                    -> ( b -> c -> ErrIO d) -> IO ()
test2FileIO progName  startfile secfile resfile op = do
        when testvardebug $ putIOwords ["testVar0FileIO read text "]
        r <- runErr $  sub progName  startfile secfile resfile op
        assertEqual (Right True) r
    where
        sub progName  startfile secfile resfile op =    do
            testDataDir <- getLitTextTestDir3 progName
            f0 <- readStartFile3 testvardebug testDataDir startfile
            f2 <- readStartFile3 testvardebug testDataDir secfile
            t1 <- op  (readTestH2 startfile f0) (readTestH2 secfile f2)
            checkResult testvardebug testDataDir resfile t1



