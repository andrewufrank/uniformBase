------------------------------------------------------------------------
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
---- using uniform:
import Test.Framework
import Uniform.Pandoc 
import Uniform.DocValue

import Uniform.Error           hiding (  (<.>)  )  -- (</>)

test_readWrite = do 
    res4 <- runErr $ do 
        let pfn1 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort" 
        let pfn2 = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort2" 

        pan1 <- read8 pfn1 docValueFileType 
        write8 pfn2  docValueFileType pan1

        pan2 <- read8 pfn2 docValueFileType
        return (pan1, pan2)
    putIOwords ["test_readWrite", "\n res1\n", showT res4, "\n"]
    let Right (target3, res3) = res4
    assertEqual target3 res3


