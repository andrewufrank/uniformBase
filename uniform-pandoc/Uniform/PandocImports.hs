--------------------------------------------------------------------------
--
-- Module      :  Uniform.PandocImports
        -- von hier Pandoc spezifisches imortieren
        -- nich exportieren nach aussen 
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
            #-}

module Uniform.PandocImports
  ( module Uniform.PandocImports
  , Pandoc (..)
      
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
-- import Uniform.DocRep
-- import Uniform.ProcessPDF 

import qualified Text.Pandoc                   as Pandoc
import           Text.Pandoc        
            -- ( Pandoc(..)
            --             , ReaderOptions
            --             , Meta
            --             , MetaValue
            --             , writerHighlightStyle
            --             , writerExtensions
            --             , WriterOptions 
            --             -- , writeMarkdown
            --             , writeHtml5String
            --             , writeLaTeX
            --             , def
            --             )

import           Text.Pandoc.Shared             ( stringify )
import           Text.Pandoc.Highlighting       ( tango )
import qualified Text.Pandoc                   as Pandoc
import qualified Text.Pandoc.Extensions as Pandoc
instance Zeros Pandoc where
  zero = Pandoc zero zero

instance Zeros Text.Pandoc.Meta where
  zero = mempty

-- -- | Handle possible pandoc failure within the PandocIO Monad
-- unPandocM2 :: Zeros a =>  Pandoc.PandocIO a -> ErrIO a
-- unPandocM2 op1 = 
--     callIO $ do 
--       res :: Either Pandoc.PandocError a <- Pandoc.runIO op1
--       case res of 
--         Left e -> do
--             putIOwords ["unPandocM error", showT e]
--             fail (show e)
--             return zero 
--         Right a -> return a
--   `catchError` (\e -> do
--                    putIOwords ["unPandocM2 catchError", showT e]
--                    fail . t2s . unwords' $ ["unPandocM2 catchError", showT $ e]

--                  )

-- | Handle possible pandoc failure within the PandocIO Monad
unPandocM :: Pandoc.PandocIO a -> ErrIO a
unPandocM op1 =
  do
      res <- callIO $ Pandoc.runIO
        (do
             -- liftIO $putStrLn "unPandocM op"
          a <- op1 --       error "xx"
          -- liftIO $putStrLn "error xx"
          -- should be
          --     result <- P.runIO $ op
          --     rst1   <- P.handleError result
          -- and put then in the two parts of ErrIO 
          return a
        )
      either
        (\e -> do
          putIOwords ["unPandocM error", showT e]
          throwError . showT $ e
        )
        return res
    `catchError` (\e -> do
                   putIOwords ["unPandocM catchError", showT e]
                   throwError . showT $ e
                 )

getMeta :: Pandoc -> Pandoc.Meta
getMeta (Pandoc.Pandoc m _) = m

putMeta :: Pandoc.Meta -> Pandoc -> Pandoc
putMeta m1 (Pandoc _ p0) = Pandoc m1 p0

-- | Flatten a Pandoc 'Meta' into a well-structured JSON object, rendering Pandoc
-- text objects into plain strings along the way.
flattenMeta :: Pandoc.Meta -> Value
flattenMeta (Pandoc.Meta meta) = toJSON $ fmap go meta
    where
        go :: MetaValue -> Value
        go (Pandoc.MetaMap     m) = toJSON $ fmap go m
        go (Pandoc.MetaList    m) = toJSONList $ fmap go m
        go (Pandoc.MetaBool    m) = toJSON m
        go (Pandoc.MetaString  m) = toJSON m
        go (Pandoc.MetaInlines m) = toJSON $ stringify m
        go (Pandoc.MetaBlocks  m) = toJSON $ stringify m

readYaml2value :: Path Abs File -> ErrIO Value

-- read a yaml file to a value
-- error when syntax issue
readYaml2value fp = do
    t <- read8 fp yamlFileType
    return . yaml2value $ t


latexOptions :: WriterOptions
latexOptions = 
    def { writerHighlightStyle = Just tango
        , writerCiteMethod = Natbib
        -- Citeproc                        -- use citeproc to render them
        --           | Natbib                        -- output natbib cite commands
        --           | Biblatex                      -- output biblatex cite commands
        , writerExtensions     =  Pandoc.extensionsFromList
                        [Pandoc.Ext_raw_tex   --Allow raw TeX (other than math)
                        -- , Pandoc.Ext_shortcut_reference_links
                        -- , Pandoc.Ext_spaced_reference_links
                        -- , Pandoc.Ext_citations           -- <-- this is the important extension for bibTex
                        ]                     
        }
-- instance ToJSON Text 
-- writeLaTeX :: PandocMonad m => WriterOptions -> Pandoc -> m Text
instance TypedFiles7 Text Text where
    wrap7 = id
    unwrap7 = id 

writeTexSnip2 ::   Pandoc -> ErrIO TexSnip
-- write a latex file from a pandoc doc 
writeTexSnip2  pandocRes = do
    p <- unPandocM $  writeLaTeX latexOptions pandocRes
    return  . TexSnip $ p

-------------------- fileType ----------
extPandoc = Extension "pandoc"

pandocFileType =
  TypedFile5 { tpext5 = extPandoc } :: TypedFile5 Text Pandoc

 
instance TypedFiles7 Text Pandoc  where
  -- handling Pandoc and read them into PandocText
  wrap7 = readNote "wrap7 for pandoc 223d" .t2s
  unwrap7   = showT

    -------------------- fileType ----------
    -- a tex snip is a piece of latex code, but not a full compilable 
    -- latex which results in a pdf 

extTexSnip = Extension "texsnip"

-- | a wrapper around TexSnip 
newtype TexSnip = TexSnip {unTexSnip ::Text}
  deriving (Show, Read, Eq, Ord)

-- unTexSnip (TexSnip a) = a   --needed for other ops

instance Zeros TexSnip where
  zero = TexSnip zero

texSnipFileType =
  TypedFile5 { tpext5 = extTexSnip } :: TypedFile5 Text TexSnip

instance TypedFiles7 Text TexSnip  where
  -- handling TexSnip and read them into TexSnipText
  -- the file on disk is readable for texstudio
  
  wrap7 = TexSnip  -- readNote "wrap7 for TexSnip dwe11d" .t2s
  unwrap7   = unTexSnip -- showT

