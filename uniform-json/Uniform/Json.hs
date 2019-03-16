-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Json
--
-- | the operations on JSON data types 
-----------------------------------------------------------------------------
{-# LANGUAGE
     MultiParamTypeClasses
    , FlexibleInstances
    , FlexibleContexts
    , UndecidableInstances
    , ScopedTypeVariables
    , DeriveDataTypeable   -- needed
    , TypeSynonymInstances
--    , DoAndIfThenElse
--    , TypeFamilies
--    , ConstraintKinds
--    , BangPatterns
    , OverloadedStrings
             #-}


module Uniform.Json (
        module Uniform.Json 
        , module Uniform.Error   -- or at least ErrIO
        , Value (..)
        , toJSON, toJSONList 
        , merge_aeson
          )  where

import Uniform.Error hiding (at)

import           Data.Aeson
import           Data.Aeson.Lens
import           Control.Lens                   ( (^?)
                                                , (?~)
                                                , (&)
                                                , at
                                                )

import qualified Data.HashMap.Lazy             as HML
                                                
class AtKey vk v where
    getMaybeStringAtKey :: vk -> Text -> Maybe v
    putStringAtKey :: Text -> v -> vk -> vk

instance AtKey Value Text where
    getMaybeStringAtKey meta2 k2 =   meta2 ^? key k2 . _String
    putStringAtKey  k2 txt meta2 = meta2 & _Object . at k2 ?~ String  txt
--        (unHTMLout text2)
instance AtKey Value Bool where
    getMaybeStringAtKey meta2 k2 =   meta2 ^? key k2 . _Bool
    putStringAtKey  k2 txt meta2 = meta2 & _Object . at k2 ?~ Bool  txt


merge_aeson :: [Value] -> Value
-- The (left-biased) union of two maps.
-- It prefers the first map when duplicate keys are encountered,
-- http://hackage.haskell.org/package/hashmap-1.3.3/docs/Data-HashMap.html
merge_aeson = Object . HML.unions . map (\(Object x) -> x)

