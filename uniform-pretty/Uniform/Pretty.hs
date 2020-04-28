-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Pretty
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


module Uniform.Pretty (module Uniform.Pretty
        )  where

import qualified Text.Show.Pretty as P 
-- import           Uniform.Strings hiding ((</>), (<.>), S)

ppShow :: Show a => a -> String
ppShow = P.ppShow 

    should use the following 
    but only if one can exclude UTCTime 
    generic instance 
class Show a => PrettyStrings a where 
    showPretty :: a -> Text
instance Show a => PrettyStrings a where
    showPretty = s2t . ppShow


