-----------------------------------------------------------------------------
--
-- Module      :  StringInfix

-- infix operations with <X> to insert X in between - unconditional
-- even if the two strings are empty
-- todo - how to compare with similar ops in fileio?

-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveGeneric  #-}
--{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -w #-}

module Uniform.Strings.LanguageCodes  where

import Uniform.Zero
import Uniform.Strings (Text, unwords', t2s)
import GHC.Generics

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
parseLanguageCode c = error . t2s . unwords' $  ["Extension.hs = parseLanguageCode ", c, "not found"]
