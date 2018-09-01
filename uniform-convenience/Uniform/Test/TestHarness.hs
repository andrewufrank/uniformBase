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


module Uniform.Test.TestHarness (module Uniform.Test.TestHarness
    , module Uniform.Error
    , module Test.Framework
    , ShowTestHarness (..)
    , FilePath
    , getLitTextTestDir3

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

{-
testVar0FileIO :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
            => Text -> a -> FilePath -> (a-> ErrIO b) -> IO ()
-- ^ a text harness for the case that the start is a value (not a file)
-- the progname gives the hidden directory of the test results (i.e. ~/.progName)
testVar1File progName  a resfile op = do
    when testvardebug $ putIOwords ["testVar0FileIO read text "
                , "with expected result ", showT resfile]
    r <- runErr $  testVar0FileIO' progName  a resfile op
    assertEqual (Right True) r
  where
    testVar0FileIO' progName  a resfile op = do
        t1 <- op a
        testDataDir <- getLitTextTestDir3 progName
        checkResult testvardebug testDataDir resfile t1

testVar0File :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
            => Text -> a -> FilePath -> (a->  b) -> IO ()
testVar0File progName  a resfile op = do
        when testvardebug $ putIOwords ["testVar0File read text "]
        let t1 = op a
        testDataDir <- getLitTextTestDir3 progName
        r <- checkResult testvardebug testDataDir resfile t1
        assertBool r

testVar2File' :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
            => Text -> a -> FilePath -> FilePath -> (a-> b -> ErrIO b) -> ErrIO Bool
-- ^ a text harness for the case that the start is a value (not a file)
-- the progname gives the hidden directory of the test results (i.e. ~/.progName)


testFile2File :: (Read a, Eq b, Show b, Read b, Zeros b
            , ShowTestHarness b, ShowTestHarness a)
            => Text -> FilePath -> FilePath -> (a->   b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- test of purecode
testFile2File  progName startfile resfile op = do
    when testvardebug $ putIOwords ["testFile2File read text for "
        , showT startfile, "with expected result ", showT resfile]
    r <- runErr $  testFile2File' progName startfile resfile op
    assertEqual (Right True) r


testFile2File' :: (Read a, Eq b, Show b, Read b, Zeros b
            , ShowTestHarness b, ShowTestHarness a)
            => Text -> FilePath -> FilePath -> (a->   b) -> ErrIO Bool
-- ^ a text harness for the transformation of data in a file to another file
-- test of purecode
testFile2File'  progName startfile resfile op = do
    testDataDir <- getLitTextTestDir3 progName
    f0 <- readStartFile3 testvardebug testDataDir startfile
--    let f1 = removeChar '\n' f0
    let tt1 =  op    (readTestH2 "read start sfadsd" $ f0)
        -- this is just a conversion to type a
    checkResult testvardebug testDataDir resfile tt1

testVar3File :: (Read a, Eq b, Show b, Read b
            , Zeros b, ShowTestHarness a, ShowTestHarness b) =>
        Text -> base -> FilePath -> FilePath -> (base -> a->   b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
testVar3File progName base startfile resfile op = do
    when testvardebug $ putIOwords ["testVar3File read text for "
        , showT startfile, "with expected result ", showT resfile]
    r <- runErr $ testVar3File' progName base startfile resfile op
    assertEqual (Right True) r

testVar3File' :: (Read a, Eq b, Show b, Read b
            , Zeros b, ShowTestHarness a, ShowTestHarness b) =>
        Text -> base -> FilePath -> FilePath -> (base -> a->   b) -> ErrIO Bool
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
testVar3File' progName base startfile resfile op = do
    testDataDir <- getLitTextTestDir3 progName
    f0 <- readStartFile3 testvardebug testDataDir startfile

    let tt1 =  op base (readTestH2 startfile f0)
    checkResult testvardebug testDataDir resfile tt1

test3File :: (Read base, Read a, Eq b, Show b, Read b, Zeros b
            , ShowTestHarness base, ShowTestHarness a, ShowTestHarness b) =>
        Text -> FilePath -> FilePath -> FilePath -> (base -> a->   b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
test3File  progName basefile startfile resfile op = do
    when testvardebug $ putIOwords ["test3File read text for "
        , showT startfile, "with expected result ", showT resfile]
    r <- runErr $ test3File'  progName basefile startfile resfile op
    assertEqual (Right True) r


test3File' :: (Read base, Read a, Eq b, Show b, Read b, Zeros b
            , ShowTestHarness base, ShowTestHarness a, ShowTestHarness b) =>
        Text -> FilePath -> FilePath -> FilePath -> (base -> a->   b) -> ErrIO Bool
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
test3File'  progName basefile startfile resfile op = do
    testDataDir <- getLitTextTestDir3 progName
    base0 <- readStartFile3 testvardebug testDataDir basefile
    f0 <- readStartFile3 testvardebug testDataDir startfile
    let base = readTestH2 "test3file readbase wer2" $ base0

    let tt1 =  op base (readTestH2 startfile f0)
    checkResult testvardebug testDataDir resfile tt1


test2FileIO :: (Read a, Eq b, Show b
                    , Read b, Zeros b, ShowTestHarness a, ShowTestHarness b) =>
          Text -> FilePath -> FilePath -> (  a-> ErrIO  b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
test2FileIO  progName startfile resfile op = do
    when testvardebug $ putIOwords ["test2FileIO read text for ", s2t startfile]
    r <- runErr $ test2FileIO'  progName startfile resfile op
    assertEqual (Right True) r


test2FileIO' :: (Read a, Eq b, Show b
                    , Read b, Zeros b, ShowTestHarness a, ShowTestHarness b) =>
          Text -> FilePath -> FilePath -> (  a-> ErrIO  b) -> ErrIO Bool
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
test2FileIO'  progName startfile resfile op = do
    testDataDir <- getLitTextTestDir3 progName
    f0 <- readStartFile3 testvardebug testDataDir startfile

    t1 <- op    (readTestH2 startfile f0)
    checkResult testvardebug  testDataDir resfile t1


testVar2FileIO :: (Read a, Eq b, Show b
                    , Read b, Zeros b, ShowTestHarness a, ShowTestHarness b) =>
        Text -> base -> FilePath -> FilePath -> (base -> a-> ErrIO  b) -> IO ()
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
testVar2FileIO progName base startfile resfile op = do
    when testvardebug $ putIOwords ["testVar2FileIO read text for ", s2t startfile]
    r <- runErr $ testVar2FileIO' progName base startfile resfile op
    assertEqual (Right True) r



testVar2FileIO' :: (Read a, Eq b, Show b
                    , Read b, Zeros b, ShowTestHarness a, ShowTestHarness b) =>
        Text -> base -> FilePath -> FilePath -> (base -> a-> ErrIO  b) -> ErrIO Bool
-- ^ a text harness for the transformation of data in a file to another file
-- with a variable as addiational arg for operation
-- test of purecode
testVar2FileIO' progName base startfile resfile op = do
    testDataDir <- getLitTextTestDir3 progName
    f0 <- readStartFile3 testvardebug testDataDir startfile

    t1 <- op base (readTestH2 "testVar2FileIO" f0)
    checkResult testvardebug  testDataDir resfile t1

-}

