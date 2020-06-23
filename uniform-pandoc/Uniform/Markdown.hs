--------------------------------------------------------------------------
--
-- Module      :  Uniform.Markdown
-------------------------------------------------------------------------
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
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports
            -fno-warn-deprecations #-}

module Uniform.Markdown
    ( module Uniform.Markdown
--   , Pandoc(..)
--   , module Uniform.Error   -- or at least ErrIO
--   , write8
--   , TypedFile5
--   , TypedFiles5
--   , TypedFiles7
--   , read8
--   , module Uniform.Json
    )
where

import           Uniform.Filenames
import           Uniform.Json
-- import           Uniform.TypedFile -- (TypedFiles7(..))
-- import           Uniform.Yaml
import           Uniform.DocRep
import           Uniform.Pandoc
-- import Uniform.Pandoc as Pandoc
import           Text.CSL.Pandoc               as Bib
import           Text.CSL                      as Pars

import           Control.Lens                   ( (^?)
                                                -- , (?~)
                                                -- , (&)
                                                -- , at
                                                )
-- import           Data.Aeson         -- for ^?, key
-- https://williamyaoh.com/posts/2019-10-19-a-cheatsheet-to-json-handling.html
-- https://artyom.me/aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import qualified Text.Pandoc                   as Pandoc
-- import qualified Text.Pandoc.Extensions                   as Pandoc
-- import qualified Data.Vector as V

-- parseArray :: Value -> Parser [(String, Bool)]
-- parseArray (Array arr) = mapM parseTuple (V.toList arr)
-- parseArray _           = fail "expected an array"

readMarkdown2docrep :: MarkdownText -> ErrIO DocRep
-- | read a md file into a DocRep
-- all values from meta are moved to yam (meta is zero to avoid problems)
readMarkdown2docrep md = do
    pd <- readMarkdown2 md
    let (Pandoc meta1 block1) = pd
    let meta2                 = flattenMeta meta1
    return (DocRep meta2 (Pandoc zero block1))
        -- zero the metadata 

docRepAddRefs :: DocRep -> ErrIO DocRep
-- ^ add the references to the pandoc block
-- the biblio is in the yam (otherwise nothing is done)
-- ths cls file must be in the yam

-- processCites :: Style -> [Reference] -> Pandoc -> Pandoc

-- Process a Pandoc document by adding citations formatted according to a CSL style. Add a bibliography (if one is called for) at the end of the document.
-- http://hackage.haskell.org/package/citeproc-hs-0.3.10/docs/Text-CSL.html
--   m <- readBiblioFile "mybibdb.bib"
--   s <- readCSLFile "apa-x.csl"
--   let result = citeproc procOpts s m $ [cites]
--   putStrLn . unlines . map (renderPlainStrict) . citations $ result

docRepAddRefs dr1@(DocRep y1 p1) = do
    putIOwords ["docRepAddRefs", showT dr1, "\n"]
    let biblio1 = getAtKey y1 "bibliography" :: Maybe Text
        style1  = getAtKey y1 "style" :: Maybe Text
        refs1   = y1 ^? key "references" :: Maybe Value
        nocite1 = getAtKey y1 "nocite" :: Maybe Text

    putIOwordsT
        [ "docRepAddRefs"
        , "\n biblio"
        , showT biblio1
        , "\n style"
        , showT style1
        , "\n refs"
        , showT refs1
        , "\n nocite"
        , showT nocite1
        ]

    let loc1  = (Just "en_US.utf8")  -- TODO depends on language

    let refs2 = fromJustNote "refs in docRepAddRefs 443" $ refs1 :: Value
    let refs3 = fromJSONValue $ refs2 -- :: Result [Reference]
    let refs4 = fromJustNote "docRepAddReffs 08werwe" refs3 :: [Reference]

    let bibliofp =
            t2s . fromJustNote "biblio2 in docRepAddRefs wer23" $ biblio1 :: FilePath
    let stylefp =
            t2s . fromJustNote "style1 in docRepAddRefs wer23" $ style1 :: FilePath

    putIOwordsT ["docRepAddRefs", "done"]

    biblio2 <- callIO $ Pars.readBiblioFile (const True) bibliofp
    style2  <- callIO $ Pars.readCSLFile loc1 stylefp

    let refsSum = refs4 ++ biblio2
    let p2      = processCites style2 refsSum p1

    putIOwordsT ["docRepAddRefs", "p2\n", showT p2]

    return (DocRep y1 p2)

fromJSONValue :: FromJSON a => Value -> Maybe a
fromJSONValue = parseMaybe parseJSON

    -- pandoc2 <- case (bibliography metaRec) of
    --     Nothing    -> return pandoc
    --     Just bibfp -> do
    --         when debug $ putIOwords
    --             [ "markdownToPandocBiblio"
    --             , "start pandocProcessCites"
    --             , showT doughP
    --             , showT bibfp
    --             , showT (bibliographyGroup metaRec)
    --             ]
    --         pandocProcessCites doughP  -- required to set the current dir 
    --                            (makeAbsFile bibfp) -- (doughP </> (makeRelFile . t2s $ bibfp))
    --                            (bibliographyGroup metaRec)
    --                            pandoc
    --        -- here the dir is used for processing in my code
    -- return pandoc2



readMarkdown2 :: MarkdownText -> ErrIO Pandoc
readMarkdown2 text1 =
    unPandocM $ Pandoc.readMarkdown markdownOptions (unwrap7 text1)
readMarkdown3 :: Pandoc.ReaderOptions -> MarkdownText -> ErrIO Pandoc
readMarkdown3 options text1 =
    unPandocM $ Pandoc.readMarkdown options (unwrap7 text1)

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
            , Pandoc.Ext_raw_tex   --Allow raw TeX (other than math)
            , Pandoc.Ext_shortcut_reference_links
            , Pandoc.Ext_spaced_reference_links
            , Pandoc.Ext_footnotes  -- all footnotes
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
        r1 <- Pandoc.writeMarkdown options -- Pandoc.def { Pandoc.writerSetextHeaders = False }
                                   dat
        return r1
    return . wrap7 $ r

-------------------- fileType ----------
-- extCSL = Extension "csl"
-- cslFileType = TypedFile5 {tpext5 = extCSL} :: TypedFile5 Text Style

-- instance TypedFiles7 Text Style where 
--     wrap7 = id 
--     unwrap7 = id 

---------------------------------
-- extBib = Extension "bib"
-- bibFileType = TypedFile5 {tpext5 = extBib}

-- instance TypedFiles7 Text 
-----------------------------
extMD = Extension "md"

newtype MarkdownText = MarkdownText Text
  deriving (Show, Read, Eq, Ord)

-- a wrapper around Markdonw text
unMT (MarkdownText a) = a   --needed for other ops

instance Zeros MarkdownText where
    zero = MarkdownText zero

markdownFileType =
    TypedFile5 { tpext5 = extMD } :: TypedFile5 Text MarkdownText


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


