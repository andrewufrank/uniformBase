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


module Uniform.ByteString_test where


import Test.Framework
import Test.Invariant
import Uniform.ByteString
--import qualified Data.ByteString as BS
--
--import System.Posix.ByteString   -- what would be the windows corresponance? --needs cygwin...
--import qualified System.Posix.FilePath as P
--import System.Posix.FilePath (RawFilePath)
--import qualified Data.Text as T
--import qualified Data.Text.IO as T
--
----import Basics  hiding ((</>), (<.>))
--import FileIO.Strings
--
--import qualified System.Directory as S (getHomeDirectory)


test_addExt = assertEqual "" (P.addExtension "\3152" "\426")
--
--prop_FP_fp2 :: Text -> Text -> Bool
---- i have no arbitrary for bytestring
---- tests fails, for input like above
--prop_FP_fp2 f e = prop_extension2 (t2b f) (t2b e)

