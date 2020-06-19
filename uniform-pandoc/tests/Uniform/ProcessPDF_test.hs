-----------------------------------------------------------------------------
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
-- --import qualified Data.Text as T
-- import qualified System.Posix  as P (FileStatus)
-- --import qualified System.Directory as S
--
---- using uniform:
import Test.Framework
import Uniform.Pandoc
import Uniform.ProcessPDF 


-- import           Uniform.Strings     hiding ((</>), (<.>))
            -- (s2t, showT, t2s, removeChar, CharChains2 (..), Text)
--import Safe   -- todo error


import Uniform.Error           hiding (  (<.>)  )  -- (</>)

-- cannot read or write pdf files ?
-- read of a pdf (displays ok) gets invalid argument (invalid byte sequence)
-- test_readWrite = do 
--     res4 <- runErr $ do 
--         let pfn1 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/someTextShort" 
--         let pfn2 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/someTextShortRWtest" 

--         pan1 <- read8 pfn1 pdfFileType 
--         write8 pfn2  pdfFileType pan1

--         pan2 <- read8 pfn2 pdfFileType
--         return (pan1, pan2)
--     putIOwords ["test_readWrite res4:", showT res4]
--     let Right (target3, res3) = res4
--     assertEqual target3 res3

test_readWriteLatex = do 
    res4 <- runErr $ do 
        let pfn1 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/someTextShort" 
        let pfn2 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/someTextShortRWtest" 

        pan1 <- read8 pfn1 texFileType 
        write8 pfn2  texFileType pan1

        pan2 <- read8 pfn2 texFileType
        return (pan1, pan2)
    let Right (target3, res3) = res4
    assertEqual target3 res3

test_tex2latex = do
    res4 <- runErr $ do
        let tsfn = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/someTextShort" 
        tex1 <- read8 tsfn texSnipFileType 
        let lat1 =  tex2latex  [tex1]
        putIOwords ["test_tex2latex", showT tex1, "\n", "lat1\n", showT lat1, "\n"]
        let latfn = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/someTextShortTarget"
        target1 :: Latex <- read8 latfn texFileType
        putIOwords ["test_tex2latex target1\n", showT target1, "\n"]
        write8 tsfn texFileType lat1
        return (target1,  lat1)
    let Right (target3, res3) = res4
    assertEqual target3 res3


-- test_latex2pdf = do
--     res4 <- runErr $ do
--         let tsfn = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/someTextShort" 
--         -- tex1 <- read8 tsfn texSnipFileType 
--         -- let lat1 =  tex2latex  tex1
--         -- putIOwords ["test_tex2latex", showT tex1, "\n", "lat1\n", showT lat1, "\n"]
--         writePDF2text True tsfn 

--         -- let latfn = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/someTextShortTarget"
--         -- target1 :: Latex <- read8 latfn texFileType
--         -- putIOwords ["test_tex2latex target1\n", showT target1, "\n"]
--         -- write8 tsfn texFileType lat1
--         return ("ok" :: Text)  -- otherwise expect error!
--     -- let Right (target3, res3) = res4
--     assertEqual (Right ("ok" :: Text)) res4

-- resok = Right ("ok" :: Text)