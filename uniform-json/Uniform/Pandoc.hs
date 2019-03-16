-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Pandoc
-----------------------------------------------------------------------------
{-# LANGUAGE
     MultiParamTypeClasses
    , FlexibleInstances
    , FlexibleContexts
    , UndecidableInstances
    , ScopedTypeVariables
    , DeriveDataTypeable   -- needed
    , TypeSynonymInstances
--    , DoAndIfThenElse
--    , TypeFamilies
--    , ConstraintKinds
--    , BangPatterns
    , OverloadedStrings
             #-}


module Uniform.Pandoc (
        module Uniform.Pandoc
        , module Uniform.Error   -- or at least ErrIO
        )  where

import           Data.Aeson (toJSONList)
import Test.Framework

-- import Data.Time as T
import           Text.Pandoc.Readers (readMarkdown)
import Uniform.Error
-- import Uniform.Strings
import Uniform.Filenames 
import Uniform.TypedFile (TypedFiles7(..), TypedFile5 (..))
import Uniform.FileIO (read8)
import Uniform.Json  

-- import qualified Data.Yaml                     as Y
-- import qualified Data.HashMap.Lazy             as HML

import   qualified        Text.Pandoc                   as Pandoc
import        Text.Pandoc  (Pandoc(..), ReaderOptions,Meta, MetaValue)
--import Text.Pandoc.Highlighting (tango)
import           Text.Pandoc.Shared             ( stringify )

extMD = Extension "md"

newtype MarkdownText = MarkdownText Text deriving (Show, Read, Eq, Ord)
-- a wrapper around Markdonw text
unMT (MarkdownText a) = a   --needed for other ops

instance Zeros MarkdownText where zero = MarkdownText zero
markdownFileType = TypedFile5 {tpext5 = extMD} :: TypedFile5   Text MarkdownText
--instance FileHandles MarkdownText
-- what is missing here?


instance TypedFiles7 Text  MarkdownText    where
-- handling Markdown and read them into MarkdownText
    wrap7 = MarkdownText
    unwrap7 (MarkdownText a) = a

readMd2meta :: Path Abs File -> ErrIO (Pandoc, Value)
-- ^ read a markdown file to metadata
readMd2meta md = do
  putIOwords ["readMd2meta", "readPandocFile", showT md]
  mdtext :: MarkdownText <- read8 md markdownFileType
  pandoc                 <- readMarkdown2 mdtext
  let meta2 = flattenMeta (getMeta pandoc)
  return (pandoc, meta2)

readMarkdown2 :: MarkdownText -> ErrIO Pandoc
readMarkdown2 (MarkdownText text1) =
  unPandocM $ readMarkdown markdownOptions text1

-- | Reasonable options for reading a markdown file
markdownOptions :: ReaderOptions
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

  -- | Handle possible pandoc failure within the Action Monad
unPandocM :: Pandoc.PandocIO a -> ErrIO a
unPandocM op1 =
  do
      res <- callIO $ Pandoc.runIO
        (do  -- liftIO $putStrLn "unPandocM op"
          a <- op1 --       error "xx"
          -- liftIO $putStrLn "error xx"
          return a
        )
      either
        (\e -> do
          putIOwords ["unPandocM error", showT e]
          throwError . showT $ e
        )
        return
        res
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
