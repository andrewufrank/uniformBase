--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
 -- {-# OPTIONS -Wall #-}


module Uniform.Convenience.DataVarious (
    showVersionT
        )   where


import           Uniform.Error
--import           Uniform.Strings
import Data.Version (showVersion)

showVersionT = s2t . showVersion 
-- ^ the version is in import           Paths_SSG (version)