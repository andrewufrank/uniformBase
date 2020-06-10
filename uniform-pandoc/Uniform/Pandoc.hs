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
      -- , readMd2meta
  , Pandoc(..)
      -- , unDocValue
  , DocValue(..)
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

-- import Text.Pandoc.Definition (Meta(..))
extMD = Extension "md"

instance Zeros Pandoc where
  zero = Pandoc zero zero

instance Zeros Text.Pandoc.Meta where
  zero = mempty

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

readMarkdown2 :: MarkdownText -> ErrIO Pandoc
readMarkdown2 text1 = unPandocM $ 
        readMarkdown markdownOptions (unwrap7 text1)
readMarkdown3 :: ReaderOptions -> MarkdownText -> ErrIO Pandoc
readMarkdown3 options text1 = unPandocM $ 
        readMarkdown options (unwrap7 text1)

writeAST2md :: Pandoc -> ErrIO MarkdownText
-- | write the AST to markdown

writeAST2md dat = do
  r <- unPandocM $ do
    r1 <- Pandoc.writeMarkdown
      Pandoc.def { Pandoc.writerSetextHeaders = False }
      dat

    return r1
  return . wrap7 $ r

writeAST3md :: WriterOptions -> Pandoc -> ErrIO MarkdownText
-- | write the AST to markdown

writeAST3md options dat = do
  r <- unPandocM $ do
    r1 <- Pandoc.writeMarkdown
      options -- Pandoc.def { Pandoc.writerSetextHeaders = False }
      dat

    return r1
  return . wrap7 $ r

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

-- | Reasonable options for rendering to HTML
html5Options :: WriterOptions
html5Options = def { writerHighlightStyle = Just tango
                   , writerExtensions     = writerExtensions def
                   }

writeHtml5String2 :: Pandoc -> ErrIO HTMLout
writeHtml5String2 pandocRes = do
    p <- unPandocM $ writeHtml5String html5Options pandocRes
    return . HTMLout $ p

newtype HTMLout = HTMLout {contentHtml::Text}
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON HTMLout

-- htmloutFileType = TypedFile5 { tpext5 = extHTML } :: TypedFile5 Text HTMLout
extTex = Extension "tex"
texFileType = TypedFile5 { tpext5 = extTex } :: TypedFile5 Text Text
    -- | Reasonable options for rendering to HTML
latexOptions :: WriterOptions
latexOptions = def { writerHighlightStyle = Just tango
                   , writerExtensions     = writerExtensions def
                   }
-- instance ToJSON Text 

-- writeLaTeX :: PandocMonad m => WriterOptions -> Pandoc -> m Text
instance TypedFiles7 Text Text where
    wrap7 = id

    unwrap7 = id 


writeLatex2text ::   Pandoc -> ErrIO Text
-- write a latex file from a pandoc doc 
writeLatex2text  pandocRes = do
    p <- unPandocM $ writeLaTeX latexOptions pandocRes
    return  p
---------- write PDF with Lualatex
-- the process uses files - is this a preformance issue? 

writePDF2text :: Bool  ->   Path Abs File -> ErrIO ()
-- convert the text in the file given (a full latex) into a pdf 
-- with the same filename
writePDF2text debug texInput  resfn =  
 do
    -- let fn1 = makeAbsFile "/home/frank/Workspace8/pandocTestLatex/example.tex"
    -- inputTex :: Text <- read8 fn1 texFileType
    putIOwords ["writePDF2text 1 tex\n", take' 300 $ texInput
                    , "resfn", showT resfn]

    let 
        texFile = concat[pre1 , lines' texInput, end9]
    putIOwords ["writePDF2text 2 texDilw\n", unlines' . take 10 $ texFile]


    -- todo replace with a temp dir 
    let tempDir = makeAbsDir "/home/frank/.SSG"
        texfn = tempDir </> (makeRelFile "writePDF2text_input")
    write8 texfn texFileType (unlines' texFile) 

    putIOwords ["writePDF2text 3 tex file", unlines' texFile]

    callIO $ callProcess "lualatex" [toFilePath texfn]

    
    let resfn = setExtension extPDF  texfn 
    putIOwords ["writePDF2text 4 pdf filename", showT resfn]

    resPDFtext <- read8 resfn pdfFileType 
    putIOwords ["writePDF2text lualatex result ok (otherwise error)"
                , "pdf is", take' 300 $ resPDFtext] 

    return ()

--- the preamble and the end -- escape \
pre1 = ["%%% eval: (setenv \"LANG\" \"en_US.UTF-8\")"
        , "\\documentclass[a4paper,10pt]{scrbook}"
        , "\\usepackage[german]{babel}"
        -- necessary for the pandoc produced TeX files: 
        , "\\usepackage[colorlinks]{hyperref}" 
        , "\\newenvironment{cslreferences}{}{\\par}"
        , "\\begin{document}"] :: [Text]

end9    = ["\\end{document}"]
----------------------------------------------

extPDF = Extension "pdf"
pdfFileType = TypedFile5 { tpext5 = extPDF } :: TypedFile5 Text Text


-- a wrapper around html ready to publish
unHTMLout (HTMLout a) = a

htmloutFileType = TypedFile5 { tpext5 = extHTML } :: TypedFile5 Text HTMLout

instance Zeros HTMLout where
  zero = HTMLout zero

instance TypedFiles7 Text HTMLout where
  wrap7 = HTMLout

  unwrap7 (HTMLout a) = a

extMD, extHTML :: Extension
extHTML = Extension "html"


applyTemplate3 :: Dtemplate -> DocValue -> ErrIO HTMLout
-- needed for old ssg lts-13.12 - also changed for 15.13

-- | apply the template in the file to the text
-- for help look in ssg master.ptpl as an example
-- the description are in doctemplates (on hackage)
applyTemplate3 templText val = do 
    temp1 :: Either String (Template Text)   <- liftIO $ DocTemplates.compileTemplate mempty (unwrap7 templText)
    -- err1 :: Either String (Doc Text) <- liftIO $ DocTemplates.applyTemplate mempty (unwrap7 templText) (unDocValue val) 
    let temp2 = case temp1 of
                Left msg -> error msg 
                Right val2 -> val2
    when False $ putIOwords ["applyTemplate3 temp2", take' 300 $ showT temp2 ]
    let res = DocTemplates.renderTemplate temp2 (unDocValue val)
    when False $ putIOwords ["applyTemplate3 res", take' 300 $ showT res ]
    let res2 =  render Nothing res 
    let res3 = HTMLout res2 
    return (res3 :: HTMLout) 

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
