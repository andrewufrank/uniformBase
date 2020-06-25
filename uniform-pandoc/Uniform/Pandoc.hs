--------------------------------------------------------------------------
--
-- Module      :  Uniform.Pandoc
        -- top import, darf nicht von andern importiert werden hier 
-------------------------------
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

{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports 
            -fno-warn-dodgy-exports 
            #-}

module Uniform.Pandoc
  ( module Uniform.Pandoc 
      , unPandocM
      , extPDF, extMD, extDocRep, extHTML, extTexSnip, extTex 
  , Pandoc(..)
  , DocRep(..)
  , module Uniform.Error   -- or at least ErrIO
  , module Uniform.Filenames 
  , write8, read8,setExtension
  , writeTexSnip2
  , pandocFileType
  , TypedFile5(..)
  , TypedFiles5(..)
  , TypedFiles7(..)
  , read8
  , module Uniform.Json
  , module Uniform.ProcessPDF
  , module Uniform.Markdown
  , module Uniform.DocRep
--   , module Uniform.BibTex
  , module Uniform.HTMLout
  , module Uniform.PandocImports
    , ReaderOptions
    , writerExtensions
    , writerHighlightStyle
    , WriterOptions(..)
  )
where

import           Uniform.Error
import           Uniform.Filenames
import           Uniform.TypedFile              ( TypedFiles7(..)
                                                , TypedFiles5(..)
                                                , TypedFile5(..)
                                                )
import           Uniform.FileIO                 ( write8
                                                , read8
                                                -- , setExtension
                                                )
import           Uniform.Json
import           Uniform.Yaml
import Uniform.DocRep
import Uniform.ProcessPDF 
import Uniform.HTMLout
import Uniform.Markdown
import Uniform.PandocImports

-- import qualified Text.Pandoc                   as Pandoc
-- import           Text.Pandoc        
--             -- ( Pandoc(..)
--             --             , ReaderOptions
--             --             , Meta
--             --             , MetaValue
--             --             , writerHighlightStyle
--             --             , writerExtensions
--             --             , WriterOptions 
--             --             -- , writeMarkdown
--             --             , writeHtml5String
--             --             , writeLaTeX
--             --             , def
--             --             )

justToKeepWarningAway :: Int 
justToKeepWarningAway = 0 