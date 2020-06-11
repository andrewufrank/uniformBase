-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Markdown
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

module Uniform.Markdown
  ( module Uniform.Markdown
      -- , readMd2meta
  , Pandoc(..)
      -- , unDocValue
--   , DocValue(..)
      -- , docValueFileType
--    , getAtKey  -- exported by uniform.json, instances automatically exported 
  , module Uniform.Error   -- or at least ErrIO
  , write8
  , TypedFile5
  , TypedFiles5
  , TypedFiles7
  , read8
--   , extTex, writeLatex2text, texFileType
--   , extPDF, pdfFileType, writePDF2text
--   , extMD
  , module Uniform.Json
--   , varListToJSON
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
import           Uniform.FileIO                 ( write8
                                                , read8
                                                , setExtension)
import           Uniform.Json
import           Uniform.Yaml
-- import Uniform.Pandoc
-- import Uniform.Pandoc as Pandoc

-- import Uniform.Doc 

-- import qualified Text.Pandoc                   as Pandoc
-- import           Text.Pandoc                    ( Pandoc(..)
--                         , ReaderOptions
--                         -- , Meta
--                         -- , MetaValue
--                         -- , writerHighlightStyle
--                         -- , writerExtensions
--                         , WriterOptions(..)
--                         -- , writeHtml5String
--                         -- , writeLaTeX
--                         -- , def
--                         -- -- , writerStandalone
--                         -- -- , Template
                        -- )
-- import           Text.Pandoc.Highlighting       ( tango )
-- -- import Text.Pandoc.PDF (makePDF)
                                                  
-- import Text.DocTemplates as DocTemplates  ( -- applyTemplate, 
--         Doc(..), renderTemplate, compileTemplate, Template)
-- import Text.DocLayout (render)
-- import           Text.Pandoc.Shared             ( stringify )
-- import System.Process 

extMD = Extension "md"

readMarkdown2 :: MarkdownText -> ErrIO Pandoc
readMarkdown2 text1 = unPandocM $ 
        readMarkdown markdownOptions (unwrap7 text1)
readMarkdown3 :: Pandoc.ReaderOptions -> MarkdownText -> ErrIO Pandoc
readMarkdown3 options text1 = unPandocM $ 
        readMarkdown options (unwrap7 text1)

    -- | Reasonable options for reading a markdown file
markdownOptions :: Pandoc.ReaderOptions
markdownOptions = Pandoc.def { Pandoc.readerExtensions = exts }
 where
  exts = mconcat
    [ Pandoc.extensionsFromList
      [ Pandoc.Ext_yaml_metadata_block
      , Pandoc.Ext_fenced_code_attributes
      , Pandoc.Ext_auto_identifiers
      , Pandoc.Ext_raw_html   -- three extension give markdown_strict
      , Pandoc.Ext_shortcut_reference_links
      , Pandoc.Ext_spaced_reference_links
      , Pandoc.Ext_citations           -- <-- this is the important extension for bibTex
      ]
    , Pandoc.githubMarkdownExtensions
    ]


writeAST2md :: Pandoc -> ErrIO MarkdownText
-- | write the AST to markdown

writeAST2md dat = do
  r <- unPandocM $ do
    r1 <- Pandoc.writeMarkdown
      Pandoc.def { Pandoc.writerSetextHeaders = False }
      dat

    return r1
  return . wrap7 $ r

writeAST3md :: Pandoc.WriterOptions -> Pandoc -> ErrIO MarkdownText
-- | write the AST to markdown

writeAST3md options dat = do
  r <- unPandocM $ do
    r1 <- Pandoc.writeMarkdown
      options -- Pandoc.def { Pandoc.writerSetextHeaders = False }
      dat

    return r1
  return . wrap7 $ r

    
newtype MarkdownText = MarkdownText Text
  deriving (Show, Read, Eq, Ord)

-- a wrapper around Markdonw text
unMT (MarkdownText a) = a   --needed for other ops

instance Zeros MarkdownText where
  zero = MarkdownText zero

markdownFileType =
  TypedFile5 { tpext5 = extMD } :: TypedFile5 Text MarkdownText

 --instance FileHandles MarkdownText
 -- what is missing here?
instance TypedFiles7 Text MarkdownText where
  -- handling Markdown and read them into MarkdownText
  wrap7 = MarkdownText

  unwrap7 (MarkdownText a) = a

readMd2meta :: Path Abs File -> ErrIO (Pandoc, Value)
-- ^ read a markdown file to metadata

readMd2meta md = do
  -- putIOwords ["readMd2meta", "readPandocFile", showT md]
  mdtext :: MarkdownText <- read8 md markdownFileType
  pandoc                 <- readMarkdown2 mdtext
  let meta2 = flattenMeta (getMeta pandoc)
  -- putIOwords ["readMd2meta", "readPandocFile", showT md, "done"]
  return (pandoc, meta2)
