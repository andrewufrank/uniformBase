{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitNamespaces #-}

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

-- data Aby = Aby44 Int  (Path Abs Dir)  deriving (Eq, Ord, Show, Read )