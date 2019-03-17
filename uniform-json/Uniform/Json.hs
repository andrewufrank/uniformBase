-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Json
--
-- | the operations on JSON data types 
-----------------------------------------------------------------------------
-- {-# LANGUAGE BangPatterns                   #-}
{-# LANGUAGE ConstraintKinds                #-}
-- {-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DoAndIfThenElse                #-}
{-# LANGUAGE FlexibleContexts               #-}
{-# LANGUAGE FlexibleInstances              #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
-- {-# LANGUAGE OverloadedStrings              #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE TypeFamilies                   #-}
-- {-# LANGUAGE TypeSynonymInstances          #-}
{-# LANGUAGE UndecidableInstances           #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Uniform.Json
    ( module Uniform.Json
    , module Uniform.Error   -- or at least ErrIO
    , Value(..)
    , ToJSON(..), FromJSON(..) 
    , fromJSON
    , decode, omitNothingFields
    )
where

import           Control.Lens                   ( (^?)
                                                , (?~)
                                                , (&)
                                                , at
                                                )

import           Data.Aeson
import           Data.Aeson.Lens
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.HashMap.Lazy             as HML

import           Uniform.Error           hiding ( at )

class AtKey vk v where
    getMaybeStringAtKey :: vk -> Text -> Maybe v
    putStringAtKey :: Text -> v -> vk -> vk

instance AtKey Value Text where
    getMaybeStringAtKey meta2 k2 = meta2 ^? key k2 . _String
    putStringAtKey k2 txt meta2 = meta2 & _Object . at k2 ?~ String txt
--        (unHTMLout text2)
instance AtKey Value Bool where
    getMaybeStringAtKey meta2 k2 = meta2 ^? key k2 . _Bool
    putStringAtKey k2 txt meta2 = meta2 & _Object . at k2 ?~ Bool txt


mergeAeson :: [Value] -> Value
-- The (left-biased) union of two maps.
-- It prefers the first map when duplicate keys are encountered,
-- http://hackage.haskell.org/package/hashmap-1.3.3/docs/Data-HashMap.html
mergeAeson = Object . HML.unions . map (\(Object x) -> x)


instance NiceStrings Value where
    shownice = bb2t . bl2b . encodePretty

