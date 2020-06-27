---------------------------------------------------------------------------
--
-- Module      :  process pdf  test
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

module Uniform.ProcessPDF_test where
--
import Test.Framework
---- using uniform:
import Uniform.Test.TestHarness
import Uniform.Pandoc
import Uniform.ProcessPDF 
import Uniform.Markdown_test 

import Uniform.Error           hiding (  (<.>)  )  -- (</>)

-- cannot read or write pdf files ?
-- read of a pdf (displays ok) gets invalid argument (invalid byte sequence)

-- test_readWriteLatex = do 
--     res4 <- runErr $ do 
--         let pfn1 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort" 
--         let pfn2 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShortRWtest" 

--         pan1 <- read8 pfn1 texFileType 
--         write8 pfn2  texFileType pan1

--         pan2 <- read8 pfn2 texFileType
--         return (pan1, pan2)
--     -- putIOwords ["test_readWriteLatex", "\n res1\n", showT res4, "\n"]
--     let Right (target3, res3) = res4
--     assertEqual target3 res3


-- test_text2latex4short = testVar0FileIO "uniform-pandoc" 
--         shortFile
--         "test_text2latex4short" text2latex4 
-- test_text2latex4reg = testVar0FileIO "uniform-pandoc" 
--         regFile
--         "test_text2latex4reg" text2latex4 
-- test_text2latex4complex = testVar0FileIO "uniform-pandoc" 
--         complexFile
--         "test_text2latex4complex" text2latex4 

-- -- testVar0FileIO :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
--             -- => Text -> a -> FilePath -> (a-> ErrIO b) -> IO ()
-- text2latex4 tsfn1  = do       
--         texsn1  <- read8 tsfn1 texSnipFileType 
--         -- let p1 = unwrap7 pan1 :: Pandoc 
--         let lat1 =  tex2latex  [texsn1] 
--         write8 tsfn1 texFileType lat1  
--         return lat1

-- instance ShowTestHarness Latex 
 

-- test_writePDF4short = testVar0FileIO "uniform-pandoc" 
--         shortFile
--         "test_writePDF4short" writePDF4 
-- test_writePDF4reg = testVar0FileIO "uniform-pandoc" 
--         regFile
--         "test_writePDF4reg" writePDF4 
-- to here commented out

test_writePDF4complex = testVar0FileIO "uniform-pandoc" 
        complexFile
        "test_writePDF4complex" writePDF4 

writePDF4 tsfn1  = do       
        pan1  <- read8 tsfn1 texFileType 
        -- let p1 = unwrap7 pan1 :: Pandoc 
        let cwd1 = makeAbsDir . getParentDir $ tsfn1 :: Path Abs Dir 
        writePDF2 True tsfn1 tsfn1 cwd1
        -- writes the pdf file
        -- pdf1 <- read8 tsfn1 pdfFileType  -- read8 cannot read pdf
        return . PDFfile $ "ok"

instance ShowTestHarness PDFfile