-----------------------------------------------------------------------------
--
-- Module      :  Uniform.regex
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


module Uniform.Regex (module Uniform.Regex
        , (=~)
        )  where

import           Uniform.Strings 
import Text.Regex.XMLSchema.Generic
import Text.Regex.TDFA   
import Text.Regex.TDFA.Text ()


splitWithPrefix :: Text -> Text -> (Text, Text)
splitWithPrefix p t = split p t 

