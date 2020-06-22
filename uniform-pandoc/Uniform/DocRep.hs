---------------------------------------------------------------------------
--
-- Module      :  Uniform.DocRep  

    -- the abstract representation of the documents 
    -- consists of pandoc for text and 
    --              metajson for all other values
    -- the text content is not in the metajson 
    -- (but can be put into the json )
    -- metajson is just a wrapped json 
    
    -- DocRep replaces DocRep 
    -- metajson replaces metarec
    -- DocRep can be read8/write8 
-----------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports 
            #-}

module Uniform.DocRep
    ( module Uniform.DocRep
        -- , Dtemplate
        -- , Template 
        -- , renderTemplate 
    )
where

import           Uniform.Error
import           Uniform.Filenames
import           Uniform.TypedFile  ( TypedFiles7(..)
                                    , TypedFiles5(..)
                                    , TypedFile5(..)
                                    )
import           Uniform.Json
import Uniform.Pandoc 
import Uniform.Json 
-- import Text.JSON

-- | a more uniform method to represent a document
-- the yam part contains the json formated yaml metadata
-- which is extensible
-- Attention the Pandoc is Pandoc (Meta (Map Text MetaValue) [Block]
-- means that title etc is duplicated in the Meta part.
-- it would be better to use only the block and all 
-- metadata keept in the yam json 
-- TODO replace Pandoc with Block in DocRep 
data DocRep = DocRep {yam:: Value, pan:: Pandoc}  -- a json value
        deriving (Show, Read, Eq)
instance Zeros DocRep where zero = DocRep zero zero

-- type Value  = LazyByteString
--------------------------------------------typed file DocRep

docrepExt = Extension "docrep"

-- instance NiceStrings DocRep where
--   shownice = showNice . unDocRep

docRepFileType :: TypedFile5 Text DocRep
docRepFileType =
  TypedFile5 { tpext5 = docrepExt } :: TypedFile5 Text DocRep

instance TypedFiles7 Text DocRep
     where
        wrap7 = readNote "DocRep wrap7 sfasdwe" . t2s
        unwrap7 = showT

mergeAll :: DocRep -> [Value] -> DocRep
-- ^ merge the values with the values in DocRec -- last winns
-- issue how to collect all css?

mergeAll (DocRep y p) vs = DocRep (mergeAeson . reverse $  y : vs) p

instance AtKey DocRep Text where
  getAtKey dr k2 = getAtKey (yam dr) k2

  putAtKey k2 txt (DocRep y p) = DocRep (putAtKey k2 txt y) p

-- instance AtKey DocRep Bool where
--   getAtKey dr k2 = getAtKey (yam dr) k2

--   putAtKey k2 b dr = DocRep $ putAtKey k2 b (unDocRep meta2)
