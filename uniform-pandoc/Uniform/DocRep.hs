---------------------------------------------------------------------------
--
-- Module      :  Uniform.DocRep  

    -- the abstract representation of the documents 
    -- consists of pandoc for text and 
    --              metajson for all other values
    -- the text content is not in the metajson 
    -- (but can be put into the json )
    -- metajson is just a wrapped json 
    
    -- DocRep replaces DocVal (Value is in json used) 
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
{-# LANGUAGE DuplicateRecordFields #-}
-- {-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports 
            -fno-warn-unused-matches 
            #-}

module Uniform.DocRep
    ( module Uniform.DocRep
    , HTMLout
    , htmloutFileType
        -- , Dtemplate
        -- , Template 
        -- , renderTemplate 
    )
where

import GHC.Generics
import           Uniform.Error
import           Uniform.Filenames
import           Uniform.TypedFile  ( TypedFiles7(..)
                                    , TypedFiles5(..)
                                    , TypedFile5(..)
                                    )
import           Uniform.Json
import Uniform.PandocImports
import Uniform.Json 
import Uniform.HTMLout
-- import Uniform.Markdown

import qualified Text.Pandoc                   as Pandoc
import           Text.CSL.Pandoc               as Bib
import           Text.CSL                      as Pars
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Control.Lens                   ( (^?)
                                                -- , (?~)
                                                -- , (&)
                                                -- , at
                                                )

-- import Text.JSON  
fromJSONValue :: FromJSON a => Value -> Maybe a
fromJSONValue = parseMaybe parseJSON


-- | a more uniform method to represent a document
-- the yam part contains the json formated yaml metadata
-- which is extensible
-- Attention the Pandoc is Pandoc (Meta (Map Text MetaValue) [Block]
-- means that title etc is duplicated in the Meta part.
-- it would be better to use only the block and all 
-- metadata keept in the yam json 
-- TODO replace Pandoc with Block in DocRep 
data DocRep = DocRep {yam:: Value, pan:: Pandoc}  -- a json value
        deriving (Show, Read, Eq, Generic)
instance Zeros DocRep where zero = DocRep zero zero
instance FromJSON DocRep 
instance ToJSON DocRep 

docRep2texsnip :: DocRep -> ErrIO TexSnip
-- ^ transform a docrep to a texsnip 
-- does not need the references include in docRep
-- TODO needed!-- which is done by tex to pdf conversion
docRep2texsnip dr1@(DocRep y1 p1) = do 
    
    ts1 :: Text <- unPandocM $ Pandoc.writeLaTeX latexOptions p1 
    return $ TexSnip  y1 ts1

-- latexOptions :: WriterOptions  -- is Pandoc
-- latexOptions = 
--     def { writerHighlightStyle = Just tango
--         , writerExtensions     =  Pandoc.extensionsFromList
--                         [Pandoc.Ext_raw_tex   --Allow raw TeX (other than math)
--                         -- , Pandoc.Ext_shortcut_reference_links
--                         -- , Pandoc.Ext_spaced_reference_links
--                         -- , Pandoc.Ext_citations           -- <-- this is the important extension for bibTex
--                         ]                     
--         }


------------------------------------

docRep2html:: DocRep -> ErrIO HTMLout
-- ^ transform a docrep to a html file 
-- needs teh processing of the references with citeproc
docRep2html dr1@(DocRep y1 p1) = do 
    dr2 <- docRepAddRefs dr1 
    h1 <- unPandocM $ writeHtml5String html5Options (pan dr2)
    return . HTMLout $  h1

--------------------------------
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
    -- the biblio entry is the signal that refs need to be processed 
    -- only refs do not work 
    putIOwords ["docRepAddRefs", showT dr1, "\n"]
    let biblio1 = getAtKey y1 "bibliography" :: Maybe Text
    maybe (return dr1) (addRefs2 dr1) biblio1 

addRefs2 dr1@(DocRep y1 p1) biblio1 = do 
    when False $ putIOwords ["addRefs2", showT dr1, "\n"]   
    let  
        style1  = getAtKey y1 "style" :: Maybe Text
        refs1   = y1 ^? key "references" :: Maybe Value -- is an array 
        nocite1 = getAtKey y1 "nocite" :: Maybe Text
                     
    when False $ putIOwordsT
        [ "docRepAddRefs"
        , "\n biblio"
        , showT biblio1 -- is only biblio "resources/BibTexLatex.bib"  
        , "\n style"
        , showT style1 -- style Just "/home/frank/Workspace8/ssg/docs/site/dough/resources/chicago-fullnote-bibliography-bb.csl"
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
            t2s  biblio1 :: FilePath
    let stylefp =
            t2s . fromJustNote "style1 in docRepAddRefs wer23" $ style1 :: FilePath
--  Raised the exception:
-- ["runErr2action","Safe.fromJustNote Nothing, style1 in docRepAddRefs wer23\nCallStack (from HasCallStack):\n  fromJustNote, called at ./Uniform/DocRep.hs:165:19 in uniform-pandoc-0.0.2-CQ6TrBvcdAe7Crud3c6Rca:Uniform.DocRep"]
    when False $ putIOwordsT ["docRepAddRefs", "done"]

    biblio2 <- callIO $ Pars.readBiblioFile (const True) bibliofp
    style2  <- callIO $ Pars.readCSLFile loc1 stylefp

    let refsSum = refs4 ++ biblio2
    let p2      = processCites style2 refsSum p1

    when False $ putIOwordsT ["docRepAddRefs", "p2\n", showT p2]

    return (DocRep y1 p2)
--------------------------------------------typed file DocRep

extDocRep = Extension "docrep"

-- instance NiceStrings DocRep where
--   shownice = showNice . unDocRep

docRepFileType :: TypedFile5 Text DocRep
docRepFileType =
  TypedFile5 { tpext5 = extDocRep } :: TypedFile5 Text DocRep

instance TypedFiles7 Text DocRep
     where
        wrap7 = readNote "DocRep wrap7 sfasdwe" . t2s
        unwrap7 = showT

mergeAll :: DocRep -> [Value] -> DocRep
-- ^ merge the values with the values in DocRec -- last winns
-- issue how to collect all css?

mergeAll (DocRep y p) vs = DocRep (mergeRightPref $  y : vs) p

instance AtKey DocRep Text where
  getAtKey dr k2 = getAtKey (yam dr) k2

  putAtKey k2 txt (DocRep y p) = DocRep (putAtKey k2 txt y) p

-- instance AtKey DocRep Bool where
--   getAtKey dr k2 = getAtKey (yam dr) k2

--   putAtKey k2 b dr = DocRep $ putAtKey k2 b (unDocRep meta2)
