-----------------------------------------------------------------------------
--
-- Module      :  Uniform.regex_test
--
-- | import regexs to test with  {-@ HTF_TESTS @-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}


module Uniform.Regex_test where

import           Test.Framework
import           Uniform.Strings hiding ((</>), (<.>), (<|>))
import   Uniform.Regex

import Control.Exception

s1 = "alle meine Entchen"
p1 = "alle"
r1 = ("alle", " meine Entchen")
test_1 = assertEqual r1 (splitWithPrefix p1 s1)


