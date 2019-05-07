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

-- import Test.Invariant
-- import Uniform.Filenames

test_temp4 = do 
    res <- runErr $ applyTemplate4 tmp1 ( vals1 )
    putIOwords ["test_temp4", "template", tmp1 
                , "\n", "vals", showNice vals1
                , "\n", "result", showT res]
    assertEqual (Right res4) res 

tmp1, res4 :: Text 
tmp1 = "some $words$ are replaced $if(x1)$the text for x1 $x1$ $endif$."
vals1 = [("words","Woerter"), ("x1","erstes x")]
res4 = "some Woerter are replaced the text for x1 erstes x ."

