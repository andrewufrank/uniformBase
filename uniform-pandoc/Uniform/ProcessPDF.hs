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

{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports 
            -fno-warn-unused-matches 
            #-}

module Uniform.ProcessPDF
  ( module Uniform.ProcessPDF
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

-- import           Uniform.Error
-- import           Uniform.Filenames
-- import           Uniform.TypedFile              ( TypedFiles7(..)
--                                                 -- , TypedFiles5(..)
--                                                 , TypedFile5(..)
--                                                 )
-- import           Uniform.Json
import Uniform.Pandoc 

import qualified System.Process as Sys 

tex2latex :: [TexSnip] -> Latex 
-- ^ combine a snipped (produced from an md file) with a preamble to 
-- produce a compilable latex file.
tex2latex snips = Latex . concat' $ [unlines' preamble1, concat' (map unTexSnip snips), unlines' postamble1]

preamble1 = [
    -- "%%% eval: (setenv \"LANG\" \"en_US.utf8\")",
    "\\documentclass[a4paper,10pt]{scrbook}",  
    "\\usepackage{fontspec}",
    -- "\\setsansfont{CMU Sans Serif}%{Arial}",
    -- "\\setmainfont{CMU Serif}%{Times New Roman}",
    -- "\\setmonofont{CMU Typewriter Text}%{Consolas}",
    "\\usepackage[ngerman]{babel}",
    "\\usepackage{graphicx}",
    "\\usepackage{makeidx}",
    "\\makeindex",
    "\\usepackage[colorlinks]{hyperref}",
    "\\providecommand{\\tightlist}{%",
        "\\setlength{\\itemsep}{0pt}\\setlength{\\parskip}{0pt}}",
    "\\begin{document}",""
    ] :: [Text]

postamble1 = [ "", "", "\\printindex",    
                "\\end{document}" ] :: [Text]

newtype Latex = Latex {unLatex::Text}
    deriving (Eq, Ord, Read, Show)
-- this is a full file, not just a snippet

extTex = Extension "tex"
texFileType = TypedFile5 { tpext5 = extTex } :: TypedFile5 Text Latex
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

-- writeTexSnip2text ::   Pandoc -> ErrIO Text
-- -- write a latex file from a pandoc doc 
-- seems not to give a full tex and thus not processing
-- writeTexSnip2text  pandocRes = do
--     p <- unPandocM $ writeLaTeX latexOptions pandocRes
--     return  p
---------- write PDF with Lualatex
-- the process uses files - is this a preformance issue? 

writePDF2text :: Bool  ->   Path Abs File -> Path Abs File -> ErrIO ()
-- convert the text in the file given (a full latex) into a pdf 
-- in the second path 
writePDF2text debug fn fnres = do 

    -- -- check for locale 
    -- loc <- callIO $ Sys.callProcess "locale" []
    -- putIOwords ["writePDF2text locale " ]

    -- process
 
    let infn = setExtension extTex fn :: Path Abs File 
    putIOwords ["writePDF2text 1 infn"] -- , showT infn]
    let dir1 = getParentDir fnres ::  FilePath 
    let out1 = "--output-directory=" <> ( dir1)
    putIOwords ["writePDF2text 2 out1", showT out1]
    callIO $ Sys.callProcess "xelatex" [out1, toFilePath infn]
    -- callIO $ Sys.callProcess "lualatex" [out1, toFilePath infn]

    -- does not work to read pdf.
    -- the files written seem ok
    -- let resfn = setExtension extPDF  fn 
    -- putIOwords ["writePDF2text 4 pdf filename", showT resfn]

    -- resPDFtext :: pdfFileType <- read8 resfn pdfFileType 
    -- putIOwords ["writePDF2text lualatex result ok (otherwise error)"
    --             , "pdf is", take' 300 . unwrap7 $ resPDFtext] 

    return ()

----------------------------------------------

extPDF = Extension "pdf"
pdfFileType = TypedFile5 { tpext5 = extPDF } :: TypedFile5 Text PDFfile

newtype PDFfile = PDFfile {unpdffile :: Text }
    deriving (Eq, Ord, Read, Show)
instance Zeros PDFfile where
  zero = PDFfile zero

instance TypedFiles7 Text PDFfile where 
    wrap7 = PDFfile
    unwrap7 = unpdffile 