-----------------------------------------------------------------------------
--
-- Module      :  piped
-- Copyright   :  andrew u frank -
--
-- | the recursive access to many files not blocking
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE
    MultiParamTypeClasses
--    , TypeSynonymInstances
    , FlexibleInstances
    , FlexibleContexts
    , ScopedTypeVariables
    , UndecidableInstances
    , OverloadedStrings
--    , TypeFamilies
    #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module  Uniform.Piped ( getRecursiveContents


            ) where

import qualified Pipes as Pipe
import  Pipes ((>->))
import qualified Pipes.Prelude as PipePrelude
--import Control.Monad (forM_)

--import System.Directory (doesDirectoryExist, getDirectoryContents)
--import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Environment (getArgs)
--import System.FilePath ((</>))
--import System.IO (openFile, IOMode (..), hClose)

---- using uniform:
import Uniform.Error
--import Uniform.Zero
import Uniform.Strings hiding ((<.>), (</>))
--
--import FileIO.Filenames
--import Uniform.FileIO
import Uniform.FileStrings
import Uniform.Filenames
import Data.List (sort)
import qualified Path.IO  (searchable, readable)

--import Test.Framework
--import Test.Invariant
--

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

                    Prelude.mapM_ (Pipe.yield) (sort files)
--                                (Path.IO.sort (map unPath files))
                    Prelude.mapM_ (getRecursiveContents) (sort dirs)
--                            (Path.IO.sort (map unPath dirs))
                    return ()--    where processOneFile fp = Pipe.yield fp








-------------------old
--getRecursiveContents :: LegalPathname -> Producer LegalPathname  ErrIO ()
--getRecursiveContents topPath = do
--
----  lift $ putIOwords ["getRecursiveContents", showT topPath]
--  properNames <- lift $ getDirContentNonHidden topPath
--  -- lift into Producer (ie. proxy)
----  let properNames = filter (`notElem` [".", ".."]) names
--  forM_ properNames $ \name -> do
--    let path = topPath </> name
--    isLink <- lift $ checkSymbolicLink path
--    if isLink then return ()
--        else do
--            isDirectory <- lift $ doesDirExist path
--            isReadExecutable <- lift $ getFileAccess path (True, False, True)
--            if isDirectory && isReadExecutable
--              then   getRecursiveContents path
--              else  do
--                    isReadable <- lift $ getFileAccess path (True, False, False)
----                    lift $ putIOwords ["getRecursiveContents isReadable"
----                                    , showT isReadable, showT path]
--                    if isReadable
--                            then yield path
--                            else return ()
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
--pipedDoIO :: LegalPathname -> LegalPathname -> (LegalPathname -> ErrIO Text) -> ErrIO ()
---- | write to the first filename the operation applied to the dir tree in the second
---- first path must not be non-readable dir or
--pipedDoIO file path transf =  do
--  hand <-   openFile file WriteMode
--  runEffect $
--    getRecursiveContents path
--    >-> P.mapM (fmap t2s . transf)
----    >-> P.stdoutLn
--    >-> P.toHandle hand
--  closeFile2 hand


