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
          )  where

import Uniform.Error hiding (at)

import           Data.Aeson
import           Data.Aeson.Lens
import           Control.Lens                   ( (^?)
                                                , (?~)
                                                , (&)
                                                , at
                                                )

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

