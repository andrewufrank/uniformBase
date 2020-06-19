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

-- import           Uniform.Strings     hiding ((</>), (<.>))
            -- (s2t, showT, t2s, removeChar, CharChains2 (..), Text)
--import Safe   -- todo error


import Uniform.Error           hiding (  (<.>)  )  -- (</>)

-- test for text to pandoc 

-- import Test.Invariant
-- import Uniform.Filenames

test_readPandoc2 = do
    st <- read8 mdFileType (makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/someText.md")
    res1 <- runErr $ readMarkdown2 ptext1
    putIOwords ["ptext1", ptext1 
                , "\n", "res1", showNice res1]
                -- , "\n", "result", showT res]
    assertEqual (Right res4text1) res1 

res4text1 = "ttxx   " :: Text
 