---------------------------------------------------------------------------
--
-- Module      :  pandoc test
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
--{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.Pandoc_test where

import Test.Framework
---- using uniform:
import Uniform.Pandoc 
import Uniform.Filenames 
import Uniform.Test.TestHarness
import Uniform.Markdown_test 
import Uniform.Error           hiding (  (<.>)  )  -- (</>)

tmp1, res4 :: Text 
tmp1 = "some $words$ are replaced $if(x1)$the text for x1 $x1$ $endif$."
vals1 = [("words","Woerter"), ("x1","erstes x")]
res4 = "some Woerter are replaced the text for x1 erstes x ."

test_readWritePandoc = do 
    res5 <- runErr $ do 
        let pfn1 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort.pandoc" 

        let pfn2 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort2.pandoc" 

        pan1 :: Pandoc <- read8 pfn1 pandocFileType 
        -- let p1 = unwrap7 pan1 :: Pandoc 

        write8 pfn2  pandocFileType pan1
        pan2 <- read8 pfn2 pandocFileType
        return (pan1, pan2)
    putIOwords ["test_readWrite", "\n res1\n", showT res5, "\n"]
    let Right (target3, res3) = res5
    assertEqual target3 res3


test_writeTexSnip2short = testVar0FileIO "uniform-pandoc" 
        shortFile
        "test_writeTexSnip2short" writeTexSnip4 
test_writeTexSnip2reg = testVar0FileIO "uniform-pandoc" 
        regFile
        "test_writeTexSnip2reg" writeTexSnip4 
test_writeTexSnip2complex = testVar0FileIO "uniform-pandoc" 
        complexFile
        "test_writeTexSnip2complex" writeTexSnip4 
test_writeTexSnip2withRef = testVar0FileIO "uniform-pandoc" 
        withRef
        "test_writeTexSnip2withRef" writeTexSnip4 

-- testVar0FileIO :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
            -- => Text -> a -> FilePath -> (a-> ErrIO b) -> IO ()
writeTexSnip4 pfn1  = do       
        pan1 :: Pandoc <- read8 pfn1 pandocFileType 
        -- let p1 = unwrap7 pan1 :: Pandoc 
        tex1 :: TexSnip <- writeTexSnip2 pan1 
        write8 pfn1 texSnipFileType tex1
        return tex1

-- test_pdf1 = testVar0File "uniform-pandoc" shortFile 
--                 "test_writePDF2short" writePDF4 

-- writePDF4 pfn1  = do       
--         let fnin = replaceExtension pfn1 extTex 
--         let fnout = replaceExtension pfn1 extPDF 
--         -- let p1 = unwrap7 pan1 :: Pandoc 
--         res <- writePDF2 True fnin fnout 
--         putIOwords ["writePDF4 res", showT res]
--         return res
instance ShowTestHarness TexSnip 