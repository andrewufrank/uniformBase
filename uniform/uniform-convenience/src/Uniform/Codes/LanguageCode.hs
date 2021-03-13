-----------------------------------------------------------------------------
--
-- Module      :  Uniform.LanguageCode
--
-- | use the iso639 codes from the basic module
    -- not currently practical -- later
--    adaption to the functions used


--------------------------------------------------------------------------
{-# LANGUAGE BangPatterns          #-}
--{-# LANGUAGE DeriveDataTypeable    #-}
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
{-# LANGUAGE DeriveGeneric  #-}

{-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is depreceiated but used in monads-tf
{-# OPTIONS_GHC -w #-}
{-# OPTIONS_HADDOCK ignore-exports #-}

module Uniform.Codes.LanguageCode (module Uniform.Codes.LanguageCode
    , module Uniform.Error

        )  where

-- import           Safe
import Uniform.FileIO
import Uniform.Error  hiding ((</>), (<.>)) -- to allow export
import Uniform.Test.Utils
import GHC.Generics

import Data.LanguageCodes

data LanguageCode = NoLanguage | German | USenglish | English
    | French | Spanish | Italian   deriving (Show, Read, Eq, Ord, Generic)
instance Zeros LanguageCode where zero = NoLanguage

parseLanguageCode :: Text -> LanguageCode
parseLanguageCode "de" = German
parseLanguageCode "deu" = German
parseLanguageCode "en" = English
parseLanguageCode "fr" = French
parseLanguageCode "fre" = French
parseLanguageCode "sp" = Spanish
parseLanguageCode "spa" = Spanish   -- correct?
parseLanguageCode "it" = Italian
parseLanguageCode "ita" = Italian  -- correct?
parseLanguageCode "xx" = NoLanguage
parseLanguageCode "xxx" = NoLanguage
parseLanguageCode c = errorT ["Extension.hs = parseLanguageCode ", c, "not found"]

giveCode :: LanguageCode -> Text
-- produce the 2 character language code w3c
giveCode NoLanguage = "xx"  -- error "giveCode for zero - nolanguage"
giveCode German     = "de"
giveCode USenglish  = "us"
giveCode English    = "en"
giveCode French    = "fr"
giveCode Spanish    = "sp"
giveCode Italian    = "it"
giveCode s          = error ("giveCode 2chars to" ++ show s)

giveCode3 :: LanguageCode -> Text
-- ^ produce the three character language code for wordnet
giveCode3 NoLanguage = "xxx" -- error "giveCode3 for zero - nolanguage"
giveCode3 German     = "deu"
-- giveCode3 USenglish  = "us"
giveCode3 English    = "eng"
giveCode3 French    = "fre"
giveCode3 Spanish    = "spa"
giveCode3 Italian    = "ita"
giveCode3 s          = error ("giveCode 3 chars to" ++ show s)

readLanguageCode :: Text -> Text -> LanguageCode
-- ^ read the code for the language German, Deutsch, Englisch
--readLanguageCode  = readNoteT
-- todo change for not case sen
readLanguageCode _ "Deutsch" = German
readLanguageCode _ "deutsch" = German
readLanguageCode _ "german" = German
readLanguageCode _ "english" = English
readLanguageCode _ "Englisch" = English
readLanguageCode _ "french" = French
readLanguageCode _ "spanish" = Spanish
readLanguageCode _ "italian" = Italian
readLanguageCode msg l  = readNoteT msg l
