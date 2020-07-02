--------------------------------------------------------------------------
--
-- Module      :  Uniform.ProcessPDF
---------------------------------------------------------------------------
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

-- import Uniform.Pandoc 
import           Uniform.FileIO
import           Uniform.DocRep
import           Uniform.PandocImports
import Uniform.Json

import  qualified System.Process                as Sys
import qualified System.Exit as Sys 

data LatexParam = LatexParam {latBibliography:: Maybe Text
                             , latStyle :: Maybe Text}
                             deriving (Eq, Ord, Read, Show, Generic)
instance Zeros LatexParam where zero = LatexParam zero zero 

doclatexOptions =
    defaultOptions 
        {fieldLabelModifier = t2s . toLowerStart . s2t . drop 2 }
instance ToJSON LatexParam where
    toJSON = genericToJSON doclatexOptions
instance FromJSON LatexParam where 
    parseJSON = genericParseJSON doclatexOptions 

tex2latex :: LatexParam -> [TexSnip] -> Latex
-- ^ combine a snipped (produced from an md file) with a preamble to 
-- produce a compilable latex file.
-- references are processed earlier (in  panrep)

tex2latex latpar snips =
    Latex
        . concat'
        $ [ unlines' preamble1
          , concat' (map unTexSnip snips)
        --   , unlines' $ if isZero latpar 
        --         then [""] 
        --         else makebiblio 
        --                 (fromJustNote "tex2latex 2wrqwe" $ latStyle latpar) 
        --                 (fromJustNote "tex2latex 00wr" $ latBibliography latpar)
          , unlines' postamble1
          ]

-- todo  - macche einen file 

preamble1 =
    [
    -- "%%% eval: (setenv \"LANG\" \"en_US.utf8\")",
      "\\documentclass[a4paper,10pt]{scrbook}"
    , "\\usepackage{fontspec}"
    ,
    -- "\\setsansfont{CMU Sans Serif}%{Arial}",  -- not for xetex
    -- "\\setmainfont{CMU Serif}%{Times New Roman}",
    -- "\\setmonofont{CMU Typewriter Text}%{Consolas}",
      "\\usepackage[ngerman]{babel}"
    , "\\usepackage{graphicx}"
    , "\\usepackage{makeidx}"
    , "\\usepackage{natbib}"
    , "\\makeindex"
    , "\\usepackage[colorlinks]{hyperref}"
    , "\\providecommand{\\tightlist}{%"
    , "\\setlength{\\itemsep}{0pt}\\setlength{\\parskip}{0pt}}"
    , ""
    , "\\begin{document}"
    , ""
    ] :: [Text]

postamble1 = ["", "", "\\printindex", "\\end{document}"] :: [Text]

-- makebiblio style biblio  = ["", "", "\\bibliographystyle{plainnat}"

-- TODO does not yet use style parameter
                -- , "", "\\bibliography{" <> biblio <> "}", ""]
--     \bibliographystyle{plainnat}

-- %achtung keine blanks in liste!
-- \bibliography{/home/frank/Workspace8/ssg/docs/site/baked/resources/BibTexLatex.bib}


newtype Latex = Latex {unLatex::Text}
    deriving (Eq, Ord, Read, Show)
-- this is a full file, not just a snippet

instance Zeros Latex where
    zero = Latex zero

extTex = Extension "tex"
texFileType = TypedFile5 { tpext5 = extTex } :: TypedFile5 Text Latex
    -- | Reasonable options for rendering to HTML

instance TypedFiles7 Text Latex where
    wrap7   = Latex
    unwrap7 = unLatex

-- writeTexSnip2text ::   Pandoc -> ErrIO Text
-- -- write a latex file from a pandoc doc 
-- seems not to give a full tex and thus not processing
-- writeTexSnip2text  pandocRes = do
--     p <- unPandocM $ writeLaTeX latexOptions pandocRes
--     return  p
---------- write PDF with Lualatex
-- the process uses files - is this a preformance issue? 

writePDF2 :: Bool -> Path Abs File -> Path Abs File -> Path Abs Dir -> ErrIO ()
-- convert the text in the file given (a full latex) into a pdf 
-- in the second path 
-- set the current working directory (which must be the directory 
-- from which images etc. are searched for )
writePDF2 debug fn fnres refDir = do

    -- -- check for locale 
    -- loc <- callIO $ Sys.callProcess "locale" []
    -- putIOwords ["writePDF2text locale "]
    -- ls <- callIO $ Sys.callProcess "ls" []
    -- putIOwords ["writePDF2text ls "]

    -- process

    let infn = setExtension extTex fn :: Path Abs File
    putIOwords ["writePDF2text 1 infn", showT infn
        , "\n\t fnres", showT fnres 
        , "\n\t refDir", showT refDir]
    let dir1 = getParentDir fnres :: FilePath
    let out1 = "--output-directory=" <> (dir1)
    putIOwords ["writePDF2text 2 out1", showT out1]
    callProcessWithCWD
        "lualatex"
        [out1, "-interaction=nonstopmode", toFilePath infn]
        refDir 
    -- callIO $ Sys.callProcess "xelatex" [out1,  "-interaction=nonstopmode" , toFilePath infn]
    -- callIO $ Sys.callProcess "lualatex" [out1, toFilePath infn]

    -- does not work to read pdf.
    -- the files written seem ok
    -- let resfn = setExtension extPDF  fn 
    -- putIOwords ["writePDF2text 4 pdf filename", showT resfn]

    -- resPDFtext :: pdfFileType <- read8 resfn pdfFileType 
    -- putIOwords ["writePDF2text lualatex result ok (otherwise error)"
    --             , "pdf is", take' 300 . unwrap7 $ resPDFtext] 

    return ()
--------------------------------
-- callProcess/callCommand with current working directory
-- from http://hackage.haskell.org/package/process-1.6.10.0/docs/src/System.Process.html#callProcess

-- | Creates a new process to run the specified command with the given
-- arguments, and wait for it to finish.  If the command returns a non-zero
-- exit code, an exception is raised.
--
-- If an asynchronous exception is thrown to the thread executing
-- @callProcess@, the forked process will be terminated and
-- @callProcess@ will wait (block) until the process has been
-- terminated.
--
-- @since 1.2.0.0
callProcessWithCWD :: FilePath -> [String] -> Path Abs Dir -> ErrIO ()
callProcessWithCWD cmd args cwd1 = callIO $ do
    exit_code <- 
        Sys.withCreateProcess -- "callProcess"
            (Sys.proc cmd args) { Sys.delegate_ctlc = True 
                            , Sys.cwd = (Just . toFilePath $ cwd1) } 
                                $ \_ _ _ p ->
                Sys.waitForProcess p
    case exit_code of
      Sys.ExitSuccess   -> return ()
      Sys.ExitFailure r -> fail . show $ r 

-- processFailedException :: String -> String -> [String] -> Int -> IO a
-- processFailedException fun cmd args exit_code =
--       ioError (mkIOError OtherError (fun ++ ": " ++ cmd ++
--                                      concatMap ((' ':) . show) args ++
--                                      " (exit " ++ show exit_code ++ ")")
--                                  Nothing Nothing)


-- -- wrapper so we can get exceptions with the appropriate function name.
-- withCreateProcess_
--   :: String
--   -> CreateProcess
--   -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
--   -> IO a
-- withCreateProcess_ fun c action =
--     C.bracketOnError (createProcess_ fun c) cleanupProcess
--                      (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)

-- withCreateProcess
--   :: CreateProcess
--   -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
--   -> IO a
-- withCreateProcess c action =
--     C.bracket (createProcess c) cleanupProcess
--               (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)


----------------------------------------------

extPDF = Extension "pdf"
pdfFileType = TypedFile5 { tpext5 = extPDF } :: TypedFile5 Text PDFfile

newtype PDFfile = PDFfile {unpdffile :: Text }
    deriving (Eq, Ord, Read, Show)
instance Zeros PDFfile where
    zero = PDFfile zero

instance TypedFiles7 Text PDFfile where
    wrap7   = PDFfile
    unwrap7 = unpdffile
