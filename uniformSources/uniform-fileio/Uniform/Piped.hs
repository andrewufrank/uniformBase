-----------------------------------------------------------------------------
--
-- Module      :  piped
-- Copyright   :  andrew u frank -
--
-- | the recursive access to many files not blocking
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
-- {-# LANGUAGE TypeSynonymInstances     #-}
{-# LANGUAGE UndecidableInstances     #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module  Uniform.Piped ( getRecursiveContents
--    , pipeMap, pipeStdoutLn
        , pipedDoIO


            ) where

import qualified Pipes as Pipe
import  Pipes ((>->))
import qualified Pipes.Prelude as PipePrelude
-- import System.Environment (getArgs)

---- using uniform:
import Uniform.Error
-- import Uniform.Strings hiding ((<.>), (</>))

import Uniform.FileStrings
-- import Uniform.Filenames
import Data.List (sort)
import qualified Path.IO  (searchable, readable)


getRecursiveContents :: -- (Path Abs File-> Pipe.Proxy Pipe.X () () String (ErrorT Text IO) ())
                  Path Abs Dir
                      -> Pipe.Proxy Pipe.X () () (Path Abs File) (ErrorT Text IO) ()
getRecursiveContents  fp = do
--    putIOwords ["recurseDir start", showT fp]
    perm <-Pipe.lift $ getPermissions' fp
    if not (Path.IO.readable perm && Path.IO.searchable perm)
        then Pipe.lift $ putIOwords ["recurseDir not readable or not searchable", showT fp]
        else do
            symLink <- Pipe.lift $ checkSymbolicLink fp -- callIO $ xisSymbolicLink fp
            if symLink
                then  Pipe.lift $ putIOwords ["recurseDir symlink", showT fp]
                else do
                    (dirs, files) <- Pipe.lift $ listDir'  fp
                    when False $ do
                        Pipe.lift $ putIOwords ["recurseDir files\n", showT files]
                        Pipe.lift $ putIOwords ["recurseDir directories\n", showT dirs]

                    Prelude.mapM_ Pipe.yield (sort files)
--                                (Path.IO.sort (map unPath files))
                    Prelude.mapM_ getRecursiveContents (sort dirs)
--                            (Path.IO.sort (map unPath dirs))
                    return ()--    where processOneFile fp = Pipe.yield fp


--
---- examples how to use...
--
--pipedDo :: LegalPathname -> (LegalPathname -> Text) -> ErrIO ()
--pipedDo path transf =  do
--
--  runEffect $
--    getRecursiveContents path
--    >-> P.map (t2s . transf)
--    >-> P.stdoutLn
--
--testDir = fromJustNote "testdir" $ makeLegalPath "/home/frank/Workspace8/uniform-fileio/testDirFileIO"
--test_getRec = do
--    res <- runErr $ pipedDo testDir (showT)
--    assertEqual (Right ()) res
--    -- check manually
--
--
--
--
-- a convenient function to go through a directory and
-- recursively apply a function to each
pipedDoIO :: Path Abs File -> Path Abs Dir -> (Path Abs File -> Text) -> ErrIO ()
pipedDoIO file path transf =  do
    hand <-   openFile2handle file WriteMode
    Pipe.runEffect $
                getRecursiveContents path
                >-> PipePrelude.map ( t2s . transf)  -- some IO type left?
                >-> PipePrelude.toHandle hand
    closeFile2 hand
    return ()
