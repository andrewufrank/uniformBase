{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DeriveGeneric #-}

module UniformBase 
    ( module UniformBase
    , module Uniform.Zero
    , module Uniform.Pointless
    , module Uniform.Error
    , module Uniform.Strings
    , module Uniform.FileIO
    -- , module Uniform.PathShowCase
    , module Uniform.Time
    , Generic
    -- , MonadIO
    -- , Read 
    -- , Path (..)
    ) where

import Uniform.Zero
import Uniform.Pointless
import Uniform.Strings 
import Uniform.FileIO
import Uniform.Error
import Uniform.Time 
-- import Uniform.PathShowCase
import GHC.Generics

-- TODO move 

data NoticeLevel = NoticeLevel0 | NoticeLevel1 | NoticeLevel2 deriving (Eq, Ord, Show, Read, Generic)
instance Zeros NoticeLevel where zero = NoticeLevel0 
inform, informNone, informAll :: NoticeLevel -> Bool
inform =  not . isZero
informNone = const False  -- to use with: when (informNone debug)
informAll = const True 