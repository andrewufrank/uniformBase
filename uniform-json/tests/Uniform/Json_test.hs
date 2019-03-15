-----------------------------------------------------------------------------
--
-- Module      :  FileIO.ByteString
--
-- | the implementation for filepath encoded as bytestring (RawFilePath = FilePathX)
--
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE
    MultiParamTypeClasses
    , TypeSynonymInstances
--    , FunctionalDependencies
    , FlexibleInstances
    , FlexibleContexts
--    , DeriveFunctor
    , ScopedTypeVariables
    , UndecidableInstances
    , OverloadedStrings
    , TypeFamilies
    #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}


module Uniform.Json_test where


import Test.Framework
import Test.Invariant
-- import Uniform.ByteString
--import qualified Data.ByteString as BS
--


