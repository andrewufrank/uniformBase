-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Example_test
--
-- | a miniaml set of  
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
{-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
{-# OPTIONS_GHC  -fno-warn-type-defaults #-}


module Uniform.Ftp_test where

import           Test.Framework
import           Uniform.Strings hiding ((</>), (<.>), (<|>))
import Uniform.Ftp

import Control.Exception

test_0 = assertEqual "" "x"


