-----------------------------------------------------------------------------
--
-- Module      :  complete the sentence in Defs0 mit lemma and a second PoS
-- Copyright   : af
--
-- conversin F -> G
-- is calling sentence by sentence for german lemmatization
-- if other lemmatization are necessary, then select different port

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables
--        , BangPatterns
            , UndecidableInstances
         #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -w #-}

module Uniform.Codes.LanguageCode_test
import           Test.Framework
import Uniform.Test.TestHarness
import Uniform.Zero
import Uniform.FileIO


