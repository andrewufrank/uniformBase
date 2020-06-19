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

