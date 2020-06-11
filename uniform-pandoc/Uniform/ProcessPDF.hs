-----------------------------------------------------------------------------
--
-- Module      :  Uniform.ProcessPDF
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

module Uniform.ProcessPDF
  ( module Uniform.ProcessPDF
  , Pandoc(..)
  , module Uniform.Error   -- or at least ErrIO
  , write8
  , TypedFile5
  , TypedFiles5
  , TypedFiles7
  , read8
  , module Uniform.Json
  )
where

import           Uniform.Error
import           Uniform.Filenames
import           Uniform.TypedFile              ( TypedFiles7(..)
                                                -- , TypedFiles5(..)
                                                , TypedFile5(..)
                                                )
import           Uniform.Json
import Uniform.Pandoc 

import System.Process 




newtype Latex = Latex {unLatex::Text}

extTex = Extension "tex"
texFileType = TypedFile5 { tpext5 = extTex } :: TypedFile5 Text Text
    -- | Reasonable options for rendering to HTML
-- latexOptions :: WriterOptions
-- latexOptions = def { writerHighlightStyle = Just tango
--                    , writerExtensions     = writerExtensions def
--                    }
-- instance ToJSON Text 

-- writeLaTeX :: PandocMonad m => WriterOptions -> Pandoc -> m Text
instance TypedFiles7 Text Latex where
    wrap7 = Latex
    unwrap7 = unLatex

-- writeLatex2text ::   Pandoc -> ErrIO Text
-- -- write a latex file from a pandoc doc 
-- seems not to give a full tex and thus not processing
-- writeLatex2text  pandocRes = do
--     p <- unPandocM $ writeLaTeX latexOptions pandocRes
--     return  p
---------- write PDF with Lualatex
-- the process uses files - is this a preformance issue? 

writePDF2text :: Bool  ->   Path Abs File -> ErrIO ()
-- convert the text in the file given (a full latex) into a pdf 
-- with the same filename
writePDF2text debug fn =  
 do
    let infn = setExtension extTex fn 
    putIOwords ["writePDF2text 1 infn", showT infn]

    callIO $ callProcess "lualatex" [toFilePath infn]

    
    let resfn = setExtension extPDF  fn 
    putIOwords ["writePDF2text 4 pdf filename", showT resfn]

    resPDFtext :: pdfFileType <- read8 resfn pdfFileType 
    putIOwords ["writePDF2text lualatex result ok (otherwise error)"
                , "pdf is", take' 300 . unwrap7 $ resPDFtext] 

    return ()

----------------------------------------------

extPDF = Extension "pdf"
pdfFileType = TypedFile5 { tpext5 = extPDF } :: TypedFile5 Text PDFfile

newtype PDFfile = PDFfile {unpdffile :: Text }
instance TypedFiles7 Text PDFfile where 
    wrap7 = PDFfile
    unwrap7 = unpdffile 