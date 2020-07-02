-----------------------------------------------------------------------
--
-- Module      : markdown test 
    -- tests for narkdown - docrep  
        -- docrep - panrec
-------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
--{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans
 -fno-warn-missing-signatures
 -fno-warn-missing-methods 
-fno-warn-duplicate-exports
-fno-warn-unused-imports 
#-}

module Uniform.Markdown_test where
--
--
---- using uniform:
import Test.Framework
import Uniform.Pandoc 
import Uniform.Markdown
import Uniform.DocRep
import Uniform.FileIO --(TypedFiles7(..))
import Uniform.Test.TestHarness
import Uniform.Error           hiding (  (<.>)  )  -- (</>)

    -- all filenames without extension 
shortFile  =makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort"
regFile = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someText"
complexFile = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/complex"

-- problem with readwrite tests - need two files to start 
test_readWrite = do 
    res4 <- runErr $ do 
        let pfn1 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort.md" 
        let pfn2 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort2.md" 

        pan1 <- read8 pfn1 markdownFileType 
        write8 pfn2  markdownFileType pan1

        pan2 <- read8 pfn2 markdownFileType
        return (pan1, pan2)
    let Right (target3, res3) = res4
    assertEqual target3 res3




test_readDocRepShort = testVar0FileIO "uniform-DocRep" 
        shortFile
        "test_readDocRepShort" readDocRep2 
test_readDocRepReg = testVar0FileIO "uniform-DocRep" 
        regFile
        "test_readDocRepReg" readDocRep2 
test_readDocRepComplex = testVar0FileIO "uniform-DocRep" 
        complexFile
        "test_readDocRepComplex" readDocRep2 
test_readDocRepWithRef = testVar0FileIO "uniform-DocRep" 
        withRef
        "test_readDocRepWithRef" readDocRep2 

withRef = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/withRef.md"

readDocRep2 mfn  = do       
    text1 <- read8 mfn markdownFileType 
    res1 :: DocRep <-  readMarkdown2docrep  text1 
    write8 mfn docRepFileType res1
    return res1

-- test_addRefs = do    -- is visual test   
--     res2 <- runErr $ do 
--         dr1 <- read8 withRef docRepFileType 
--         res1 <- docRepAddRefs dr1
--         putIOwords ["test_addRefs", showT res1]
--         return (res1) 
--     assertEqual (Left "") res2 
-- -- works but should be using a regression test. output is too long
 
-- res4text1 = "ttxx   " :: Text
instance ShowTestHarness Pandoc 
instance ShowTestHarness DocRep 

test_readpanrecShort = testVar0FileIO "uniform-panrec" 
        shortFile
        "test_readpanrecShort" read2panrep2 
test_readpanrecReg = testVar0FileIO "uniform-panrec" 
        regFile
        "test_readpanrecReg" read2panrep2 
test_readpanrecComplex = testVar0FileIO "uniform-panrec" 
        complexFile
        "test_readpanrecComplex" read2panrep2 
test_readpanrecwithRef = testVar0FileIO "uniform-panrec" 
        withRef
        "test_readpanrecwithRef" read2panrep2 

-- testVar0FileIO :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
            -- => Text -> a -> FilePath -> (a-> ErrIO b) -> IO ()
read2panrep2 mfn  = do       
        text1 <- read8 mfn docRepFileType 
        let res1 = Panrep (yam text1) (pan text1)
        write8 mfn panrepFileType res1
        return res1

instance ShowTestHarness Panrep