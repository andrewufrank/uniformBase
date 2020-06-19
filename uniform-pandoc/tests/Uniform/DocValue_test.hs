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

module Uniform.DocValue_test where
--
-- --import qualified Data.Text as T
-- import qualified System.Posix  as P (FileStatus)
-- --import qualified System.Directory as S
--
---- using uniform:
import Test.Framework
import Uniform.Pandoc 
import Uniform.DocValue

-- import           Uniform.Strings     hiding ((</>), (<.>))
            -- (s2t, showT, t2s, removeChar, CharChains2 (..), Text)
--import Safe   -- todo error


import Uniform.Error           hiding (  (<.>)  )  -- (</>)

test_readWrite = do 
    res4 <- runErr $ do 
        let pfn1 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/someTextShort" 
        let pfn2 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/someTextShort2" 

        pan1 <- read8 pfn1 docValueFileType 
        write8 pfn2  docValueFileType pan1

        pan2 <- read8 pfn2 docValueFileType
        return (pan1, pan2)
    let Right (target3, res3) = res4
    assertEqual target3 res3
