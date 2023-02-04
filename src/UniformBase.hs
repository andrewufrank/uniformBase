{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitNamespaces #-}

module UniformBase 
    ( module UniformBase
    , module Uniform.Zero
    , module Uniform.Pointless
    , module Uniform.Tuples
    , module Uniform.Error
    , module Uniform.NoticeLevel
    , module Uniform.Strings
    , module Uniform.FileIO
    -- , module Uniform.PathShowCase
    , module Uniform.Time
    , Generic
    -- , MonadIO
    -- , Read 
    -- , Path (..)
    ) where

-- from algebras:
import Uniform.Zero
import Uniform.Pointless
import Uniform.Tuples
--
import Uniform.Strings 
import Uniform.FileIO
import Uniform.Error
import Uniform.NoticeLevel
import Uniform.Time 
-- import Uniform.PathShowCase
import GHC.Generics

-- TODO move 

