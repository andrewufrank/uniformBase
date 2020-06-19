-----------------------------------------------------------------------------
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

module Uniform.Markdown_test where
--
-- --import qualified Data.Text as T
-- import qualified System.Posix  as P (FileStatus)
-- --import qualified System.Directory as S
--
---- using uniform:
import Test.Framework
import Uniform.Pandoc 
import Uniform.Markdown

-- import           Uniform.Strings     hiding ((</>), (<.>))
            -- (s2t, showT, t2s, removeChar, CharChains2 (..), Text)
--import Safe   -- todo error


import Uniform.Error           hiding (  (<.>)  )  -- (</>)

-- test for text to pandoc 

-- import Test.Invariant
-- import Uniform.Filenames
test_readWrite = do 
    res4 <- runErr $ do 
        let pfn1 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/someTextShort.md" 
        let pfn2 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/someTextShort2.md" 

        pan1 <- read8 pfn1 markdownFileType 
        write8 pfn2  markdownFileType pan1

        pan2 <- read8 pfn2 markdownFileType
        return (pan1, pan2)
    let Right (target3, res3) = res4
    assertEqual target3 res3


test_readPandoc2 = do
    res4 <- runErr $ do
        let mfn = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/someTextShort.md" 
        text1 <- read8 mfn markdownFileType 
        res1 :: Pandoc <-  readMarkdown2  text1
        -- putIOwords ["ptext1", showT text1, "\n", "res1\n", showT res1, "\n"]
        let pfn = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/someTextShort.pandoc"
        pres :: Pandoc <- read8 pfn pandocFileType
        return (pres,  res1)
    let Right (target3, res3) = res4
    assertEqual target3 res3

test_readPandoc2a = do
    res4 <- runErr $ do
        let mfn = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/someText.md"         
        text1 <- read8 mfn markdownFileType 
        res1 :: Pandoc <-  readMarkdown2  text1
        -- putIOwords ["ptext1", showT text1, "\n", "res1\n", showT res1, "\n"]
        let pfn = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/someText.pandoc"
        pres :: Pandoc <- read8 pfn pandocFileType
        return (pres,  res1)
    let Right (target3, res3) = res4
    assertEqual target3 res3

-- res4text1 = "ttxx   " :: Text
 