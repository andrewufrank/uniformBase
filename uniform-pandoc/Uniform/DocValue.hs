-----------------------------------------------------------------------------
--
-- Module      :  Uniform.DocValue for template mechanism in pandoc 
-- imports only DocTemplates, nothing Pandoc 
-- is imported by Uniform.Pandoc.hs
-----------------------------------------------------------------------------
-- {-# LANGUAGE BangPatterns                   #-}
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

{-# OPTIONS_GHC -Wall -fno-warn-orphans -fno-warn-missing-signatures
            -fno-warn-missing-methods -fno-warn-deprecations #-}

module Uniform.DocValue
  ( module Uniform.DocValue
    , Dtemplate
    , Template 
    , renderTemplate 

      -- , readMd2meta
--   , Pandoc(..)
--       -- , unDocValue
-- --   , DocValue(..)
--       -- , docValueFileType
-- --    , getAtKey  -- exported by uniform.json, instances automatically exported 
--   , module Uniform.Error   -- or at least ErrIO
--   , write8
--   , TypedFile5
--   , TypedFiles5
--   , TypedFiles7
--   , read8
-- --   , extTex, writeLatex2text, texFileType
-- --   , extPDF, pdfFileType, writePDF2text
-- --   , extMD
--   , module Uniform.Json
-- --   , varListToJSON
  )
where

-- import           Text.Pandoc.Readers            ( readMarkdown )
import           Uniform.Error
-- import Uniform.Pointless (cross)
import           Uniform.Filenames
import           Uniform.TypedFile              ( TypedFiles7(..)
                                                , TypedFiles5(..)
                                                , TypedFile5(..)
                                                )
-- import           Uniform.FileIO                 (  setExtension)
import           Uniform.Json
-- import           Uniform.Yaml
-- import Uniform.HTMLout


-- import qualified Text.Pandoc                   as Pandoc
-- import           Text.Pandoc                    ( Pandoc(..)
--                         , ReaderOptions
--                         , Meta
--                         , MetaValue
--                         , writerHighlightStyle
--                         , writerExtensions
--                         , WriterOptions(..)
--                         , writeHtml5String
--                         , writeLaTeX
--                         , def
--                         -- , writerStandalone
--                         -- , Template
--                         )
-- import           Text.Pandoc.Highlighting       ( tango )
-- import Text.Pandoc.PDF (makePDF)
                                                  
import Text.DocTemplates as DocTemplates  ( -- applyTemplate, 
        Doc(..)
        , renderTemplate
        -- , compileTemplate
        , Template
        )

-- import Text.DocLayout (render)
-- import           Text.Pandoc.Shared             ( stringify )
-- import System.Process 


--     renderTemplate :: (TemplateTarget a, ToContext a b) => Template a -> b -> Doc a

-- Render a compiled template in a "context" which provides values for the template's variables.

-- compileTemplate :: (TemplateMonad m, TemplateTarget a) => FilePath -> Text -> m (Either String (Template a))

-- Compile a template. The FilePath parameter is used to determine a default path and extension for partials and may be left empty if partials are not used.



unDoc2text :: Doc Text -> Text 
unDoc2text (Text _ t) = t
unDoc2text v  = error ("not a pandoc text value" ++ show v)


-- handling the doctype templates dtpl
extDtemplate :: Extension
extDtemplate = Extension "dtpl"

newtype Dtemplate = Dtemplate Text
  deriving (Show, Read, Eq, Ord)
-- ^ a template which contains variables in doctype  $x$  format

-- a wrapper around html ready to publish
--unDtemplate (Dtemplate a) = a
dtmplFileType :: TypedFile5 Text Dtemplate
dtmplFileType = makeTyped extDtemplate :: TypedFile5 Text Dtemplate

-- instance Zeros Dtemplate where
--   zero = Dtemplate zero

-- -- instance TypedFiles5 Text Dtemplate where


instance TypedFiles7 Text Dtemplate where
  wrap7 = Dtemplate

  unwrap7 (Dtemplate a) = a

newtype DocValue = DocValue Value
  deriving (Show, Eq, Read)
-- ^ a value type with "content" which is a html translation
-- and all the other keys

unDocValue :: DocValue -> Value
unDocValue (DocValue v) = v

instance Zeros DocValue where
  zero = DocValue Null

instance NiceStrings DocValue where
  shownice = showNice . unDocValue

docValueFileType :: TypedFile5 Text DocValue
docValueFileType =
  TypedFile5 { tpext5 = Extension "docval" } :: TypedFile5 Text DocValue

 --instance FileHandles MarkdownText
 -- what is missing here?
instance TypedFiles7 Text DocValue
  -- ^ handling Markdown and read them into DocValue
 where
  wrap7 = DocValue . fromJustNote "wrap7 docvalue decode" . decode . b2bl . t2b

  unwrap7 (DocValue a) = shownice a

mergeAll :: [Value] -> DocValue
-- ^ merge the four diffferent value -- last winns
-- issue how to collect all css?

mergeAll = DocValue . mergeAeson . reverse

instance AtKey DocValue Text where
  getAtKey meta2 k2 = getAtKey (unDocValue meta2) k2

  putAtKey k2 txt meta2 = DocValue $ putAtKey k2 txt (unDocValue meta2)

instance AtKey DocValue Bool where
  getAtKey meta2 k2 = getAtKey (unDocValue meta2) k2

  putAtKey k2 b meta2 = DocValue $ putAtKey k2 b (unDocValue meta2)
