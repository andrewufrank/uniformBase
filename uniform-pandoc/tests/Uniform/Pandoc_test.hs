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

module Uniform.Pandoc_test where
--
-- --import qualified Data.Text as T
-- import qualified System.Posix  as P (FileStatus)
-- --import qualified System.Directory as S
--
---- using uniform:
import Test.Framework
import Uniform.Pandoc 

-- import           Uniform.Strings     hiding ((</>), (<.>))
            -- (s2t, showT, t2s, removeChar, CharChains2 (..), Text)
--import Safe   -- todo error


import Uniform.Error           hiding (  (<.>)  )  -- (</>)

-- test for tex production 


-- import Test.Invariant
-- import Uniform.Filenames

-- test_temp4 = do 
--     res <- runErr $ applyTemplate4 tmp1 ( vals1 )
--     putIOwords ["test_temp4", "template", tmp1 
--                 , "\n", "vals", showNice vals1
--                 , "\n", "result", showT res]
--     assertEqual (Right res4) res 

-- test_temp3 = do 
--     res <- runErr $ applyTemplate3 tmp1 ( vals1 )
--     putIOwords ["test_temp4", "template", tmp1 
--                 , "\n", "vals", showNice vals1
--                 , "\n", "result", showT res]
--     assertEqual (Right res4) res 

tmp1, res4 :: Text 
tmp1 = "some $words$ are replaced $if(x1)$the text for x1 $x1$ $endif$."
vals1 = [("words","Woerter"), ("x1","erstes x")]
res4 = "some Woerter are replaced the text for x1 erstes x ."

test_readWrite = do 
    res4 <- runErr $ do 
        let pfn1 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort.pandoc" 

        let pfn2 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort2.pandoc" 

        pan1 :: Pandoc <- read8 pfn1 pandocFileType 
        -- let p1 = unwrap7 pan1 :: Pandoc 

        write8 pfn2  pandocFileType pan1
        pan2 <- read8 pfn2 pandocFileType
        return (pan1, pan2)
    putIOwords ["test_readWrite", "\n res1\n", showT res4, "\n"]
    let Right (target3, res3) = res4
    assertEqual target3 res3



test_writeTexSnip2short = do 
    res4 <- runErr $ do 
        let pfn1 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort.pandoc" 
        let tsfn1 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort.texsnip" 

        pan1 :: Pandoc <- read8 pfn1 pandocFileType 
        -- let p1 = unwrap7 pan1 :: Pandoc 
        tex1 :: TexSnip <- writeTexSnip2 pan1 

        write8 tsfn1  texSnipFileType tex1

        let tsfn2 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort2.texsnip" 
        texRes <- read8 tsfn2 texSnipFileType
        return (tex1, texRes)
    putIOwords ["test_writeTexSnip2", "\n res1\n", showT res4, "\n"]
    let Right (target3, res3) = res4
    assertEqual target3 res3

test_writeTexSnip2 = do 
    res4 <- runErr $ do 
        let pfn1 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someText.pandoc" 
        let tsfn1 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someText.texsnip" 

        pan1 :: Pandoc <- read8 pfn1 pandocFileType 
        -- let p1 = unwrap7 pan1 :: Pandoc 
        tex1 :: TexSnip <- writeTexSnip2 pan1 

        write8 tsfn1  texSnipFileType tex1

        let tsfn2 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someText2.texsnip" 
        texRes <- read8 tsfn2 texSnipFileType
        return (tex1, texRes)
    putIOwords ["test_writeTexSnip2", "\n res1\n", showT res4, "\n"]
    let Right (target3, res3) = res4
    assertEqual target3 res3

-- a pandoc text value 