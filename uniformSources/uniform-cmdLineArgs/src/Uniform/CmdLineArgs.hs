-----------------------------------------------------------------------------
--
-- Module      :  Uniform.CmdLineArgs
--
-- | a miniaml set of
-----------------------------------------------------------------------------
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
    -- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.CmdLineArgs (module Uniform.CmdLineArgs
        , (<>), (<*>), Parser (..)
        ,switch, long, short, help
        , metavar, argument, str
        , strOption, value
        , header, helper, fullDesc, progDesc, info, execParser
        )  where

import           Uniform.Strings hiding ((</>), (<.>), S)

import           Options.Applicative.Builder
import           Options.Applicative

