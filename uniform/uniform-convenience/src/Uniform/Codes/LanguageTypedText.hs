-----------------------------------------------------------------------------
--
-- Module      :  Language Typed Text
-- Copyright   :  andrew u frank -
--
-- | a text type which has as a type paramter the language


-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses
    , DeriveGeneric
    , GeneralizedNewtypeDeriving
    , TypeFamilies
    , DeriveAnyClass  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Uniform.Codes.LanguageTypedText
    (module Uniform.Codes.LanguageTypedText
    , module Uniform.Codes.LanguageCode

    ) where

import Uniform.Zero (Zeros (..), Generic(..))
import Uniform.Error -- (undef)
--import Data.RDFext.Extension (LanguageCode (..)
--            , RDFsubj, RDFproperty, Triple (..), mkTripleLang3, giveCode3)
-- import Data.Aeson (ToJSON (..), (.=), object)
import Uniform.Codes.LanguageCode
import Uniform.Json 



data LCtext = LCtext {ltxt :: Text
                        , llang :: LanguageCode
                      } deriving (Read, Show, Eq, Ord, Generic, Zeros)
--instance Zeros LCtext where
--    zero = LCtext "" NoLanguage
instance NiceStrings LCtext where
    shownice = shownice . ltxt

instance ToJSON LCtext where
    toJSON LCtext{..}   = object ["@language" .= giveCode3 llang, "@value" .= ltxt]

class LanguageCodedText l where
    codeText  :: LanguageCode-> Text -> l
    getText :: l -> Text
    setText :: Text -> l -> l
    do2text :: (Text -> Text) -> l -> l
    do2text op l = setText (op . getText $ l) l
    setLanguageCode :: LanguageCode -> l -> l
    getLanguageCode :: l -> LanguageCode
    getLengthLC :: l -> Int
    notNullLC :: l -> Bool
    sameLanguageLC :: l -> l -> Bool
    mergeLC :: Text -> l -> l -> Maybe l
    -- ^ merge with the separator between

instance LanguageCodedText LCtext where
    codeText lc t = LCtext t lc
    setLanguageCode lc2 (LCtext t lc) = LCtext t lc2
    getText = ltxt
    setText t l = l{ltxt=t}
    getLanguageCode = llang
    getLengthLC = lengthChar . getText
    notNullLC = (0 /=) . getLengthLC
    sameLanguageLC a b =  getLanguageCode a == getLanguageCode b
    mergeLC sep a b = if sameLanguageLC a b
        then Just $ a {ltxt = getText a <> sep <> getText b}
        else Nothing


--mkTripleLang33 :: RDFsubj -> RDFproperty -> LCtext -> Triple
--mkTripleLang33 o p lctext = mkTripleLang3 (getLanguageCode lctext) o p (getText lctext)


----------- manipulation of LCtext 
-- instance Zeros (LF LCtext) where 
--     -- type LF LCtext = LCtext
--     zero = codeText NoLanguage zero 

instance ListForms LCtext where 
    type LF LCtext = Text -- likely wrong?
instance Monoid LCtext  where 
        mempty = zero 
instance Semigroup LCtext 

instance CharChains LCtext where 
    null' = null' . getText   
    trim' = do2text trim'
    lengthChar = lengthChar . getText 
    isPrefixOf' t = isPrefixOf' (getText t) . getText 
    stripPrefix' t s = maybe Nothing (\x -> Just $ do2text (const x) s) res 
        where res = stripPrefix' (getText t) $ getText s
        -- unlines' = 
    -- stripPrefix' pre lct = do 
    --                 t2 <- stripPrefix' pre (getText lct)
    --                 return $ codeText (getLanguageCode lct) t2 
    -- requires for the prefix a code lang text 

