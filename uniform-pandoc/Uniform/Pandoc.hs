-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Pandoc
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
            -fno-warn-missing-methods #-}

module Uniform.Pandoc
  ( module Uniform.Pandoc
      , unPandocM
  , Pandoc(..)
  , DocValue(..)
  , module Uniform.Error   -- or at least ErrIO
  , module Uniform.Filenames 
  , write8, read8,setExtension
  , TypedFile5(..)
  , TypedFiles5(..)
  , TypedFiles7(..)
  , read8
  , module Uniform.Json
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
import Uniform.DocValue

import qualified Text.Pandoc                   as Pandoc
import           Text.Pandoc                    ( Pandoc(..)
                        , ReaderOptions
                        , Meta
                        , MetaValue
                        , writerHighlightStyle
                        , writerExtensions
                        , WriterOptions 
                        -- , writeMarkdown
                        , writeHtml5String
--                         , writeLaTeX
                        -- , def
                        )

import           Text.Pandoc.Shared             ( stringify )

instance Zeros Pandoc where
  zero = Pandoc zero zero

instance Zeros Text.Pandoc.Meta where
  zero = mempty

-- | Handle possible pandoc failure within the Action Monad
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




