-----------------------------------------------------------------------------
--
-- Module      :  Language Typed Text
-- Copyright   :  andrew u frank -
--
-- | a text type which has as a type paramter the language


-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -w #-}

module Uniform.Codes.LanguageTypedText_test where

import           Test.Framework
import Uniform.Codes.LanguageCode 
import Uniform.Codes.LanguageTypedText

test_iszero = assertBool (isZero (zero :: LCtext ))


