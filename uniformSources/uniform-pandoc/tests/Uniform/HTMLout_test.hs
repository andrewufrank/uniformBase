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

module Uniform.HTMLout_test where

import Test.Framework
import Text.DocTemplates
---- using uniform:
import Uniform.HTMLout
import Uniform.FileIO 
import Uniform.Strings
-- import Uniform.Pandoc 
-- import Uniform.Filenames 
-- import Uniform.Test.TestHarness
-- import Uniform.Markdown_test 
-- import Uniform.Error           hiding (  (<.>)  )  -- (</>)

templName = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/resources/master4.dtpl" 
test_readTempl = do 
    r1 <- runErr $ do 
        r ::   Text  <- readFile2 templName 
        -- r <- read8 templName dtmplFileType 
        putIOwords ["test_readTempl", take' 300 . showT $ r]
        -- used for compile is a text!
        
        -- let r2 = read (t2s r) :: Dtemplate
        -- putIOwords ["test_readTempl Dtemplate", take' 300 . showT $ r2]
        -- r3 :: Dtemplate <- read8 templName dtmplFileType
        -- putIOwords ["test_readTempl read8", take' 300 . showT $ r2]

        return r
    assertEqual True False  

    