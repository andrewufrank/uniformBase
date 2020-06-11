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

import           Text.Pandoc.Readers            ( readMarkdown )
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
import qualified Text.Pandoc                   as Pandoc
import           Text.Pandoc                    ( Pandoc(..)
                        , ReaderOptions
                        , Meta
                        , MetaValue
                        , writerHighlightStyle
                        , writerExtensions
                        , WriterOptions(..)
                        , writeHtml5String
                        , writeLaTeX
                        , def
                        -- , writerStandalone
                        -- , Template
                        )
import           Text.Pandoc.Highlighting       ( tango )
-- import Text.Pandoc.PDF (makePDF)
                                                  
import Text.DocTemplates as DocTemplates  ( -- applyTemplate, 
        Doc(..), renderTemplate, compileTemplate, Template)
import Text.DocLayout (render)
import           Text.Pandoc.Shared             ( stringify )
import System.Process 