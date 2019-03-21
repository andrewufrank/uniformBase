------------------------------------------------------------------------------
--
-- Module      :  FileIO.Strings
--
-- | the instance for strings (was in 0.1.1)
-- filenames are Path
-- should only export the instances
-- removed -- file content can be lazy bytestring
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS -w #-}

module Uniform.FileStrings (
             module Uniform.Filenames   -- exports Path
            , module Uniform.FileIOalgebra
            , SIO.IOMode (..)  -- from System.IO
            , closeFile2
            , listDir'
            , TIO.hGetLine, TIO.hPutStr  -- for other implementations of FileHandle
            ) where

import           Uniform.FileIOalgebra
import           Uniform.Filenames as FN
import Uniform.PathShowCase 
import           Uniform.Filenames
import           Uniform.FileStatus
-- import           Uniform.Strings hiding ((<.>), (</>))


import  qualified         Path                   as Path
import  qualified         Path.IO                as PathIO

import qualified System.IO              as SIO
import           System.Posix           (FileMode)


import qualified Data.ByteString        as BS (readFile, writeFile)
import qualified Data.ByteString.Lazy   as L
import           Data.Digest.Pure.MD5   (md5)
import           Data.Maybe             (catMaybes)
import qualified Data.Text.IO           as T (readFile, writeFile, appendFile)
import qualified Data.Text.IO           as TIO (hGetLine, hPutStr)

import qualified System.Directory       as D
import qualified System.FilePath        as OS
import qualified System.Posix           as Posix
-- for fileAccess
import           Control.Arrow          (first, second)
import           Control.DeepSeq        (force, ($!!))
import           Control.Exception      (SomeException, catch)
import           Control.Monad.Catch    as Catch
import           Control.Monad.IO.Class
import           Data.Either            (isLeft)
import           Data.List              (isPrefixOf)

closeFile2 :: SIO.Handle -> ErrIO ()
-- close a handle, does not need a filepath
closeFile2 handle = callIO $ SIO.hClose handle

instance FileHandles String where
    write2handle h c = callIO $ SIO.hPutStr h c
    readLine4handle h = callIO $ SIO.hGetLine h

instance FileHandles L.ByteString where
    write2handle h c = callIO $ L.hPutStr h c
    readLine4handle h = error "readLine4handle not implemented for lazy bytestring in FileStrings"

instance FileHandles Text where
    write2handle h c = callIO $ TIO.hPutStr h c
    readLine4handle h = callIO $ TIO.hGetLine h

instance FileHandles [Text] where
    write2handle h c = callIO $ TIO.hPutStr h (unlines' c)
    readLine4handle h = do
            res <-  callIO $ TIO.hGetLine h
            return . lines' $ res



listDir' :: (MonadIO m, MonadThrow m)
  => Path b Dir          -- ^ Directory to list
  -> m ([Path Abs Dir], [Path Abs File]) -- ^ Sub-directories and files
listDir' p =  do
    abList ::([Path.Path Abs Dir], [Path.Path Abs File]) 
                        <- PathIO.listDir . unPath $ p
    let abPathList = abList
    return abPathList
-- would require a class and an implementation for FilePath

instance FileSystemOps FilePath where
    checkSymbolicLink fp =  callIO $ D.pathIsSymbolicLink ( fp)
    getPermissions' = callIO . D.getPermissions

instance DirOps FilePath where
    doesDirExist' = callIO . D.doesDirectoryExist
    createDirIfMissing' = callIO . D.createDirectoryIfMissing True
    -- creates recusrively up

    createDir' = callIO . D.createDirectory
    renameDir' old new = do
        putIOwords ["renamed start"]
        testSource <-  doesDirExist' old
        testTarget <-  doesDirExist' new
        if testTarget then throwErrorT
--                signalf  TargetExist
                        [showT new]
          else if not testSource then throwErrorT
--                    signalf  SourceExist
                    [showT old]
                else do
                     callIO $ putStrLn "renamed"
                     r <- callIO $ D.renameDirectory (old) (new)
                     return ()
--                     $ unwordsT
--                        [ "renamed dir from ", showT old
--                            , " to " , showT  new]

    deleteDirRecursive f =
        do
            t <- doesDirExist' f
            when t $ do
                callIO . D.removeDirectoryRecursive  $  f

                putIOwords ["deleted", showT f]
            return ()

instance FileOps FilePath  where
    doesFileExist'   = callIO . D.doesFileExist
--    getPermissions' = callIO . D.getPermissions

--    createDir fp = do
--        t <- doesFileOrDirExist fp
--        if not t then  callIO $ D.createDirectory   fp
--            else throwErrorT
--                ["File or Dir exists", showT fp]


--    createDirIfMissing = callIO . D.createDirectoryIfMissing True

    copyOneFile old new = do
        t <- doesFileExist' old
        t2 <- doesFileExist' new
        if t && (not t2) then do
            let dir = getParentDir new  -- was takeDir
            direxist <- doesDirExist'   dir
            unless direxist $ do
                createDirIfMissing'  dir
            callIO $ D.copyFile (old) ( new)

                else if not t
                        then  throwErrorT
                            ["copyFile source not exist", showT old]
                            -- signalf   SourceNotExist
                        else if t2
                            then throwErrorT
                                ["copyFile target exist", showT new]
                            --   signalf  TargetExist
                            else throwErrorT ["copyOneFile", "other error"]

--    renameFile old new = do
--        t <- doesFileExist old
--        t2 <- doesFileExist new
--        if t && (not t2)
--            then do
--                let dir = takeDir new
--                direxist <- doesDirExist  dir
--                unless direxist $ do
--                    createDirIfMissing  dir
--                callIO $ D.renameFile ( old) ( new)
--
--            else if not t
--                then  throwErrorT
----                signalf   SourceNotExist
--                            ["renameFile source not exist", showT old]
--                else throwErrorT
----                            signalf  TargetExist
--                                 ["renameFile target exist", showT new]


    getMD5 fn = do
--            putIOwords ["getMD5 in FileStrings.hs", showT fn]
            status <- getSymbolicLinkStatus fn
--            let status = fromJustNote "getMD5 xx33" $ mstatus
            let regular = isRegularFile status
            readable <- getFileAccess fn (True, False, False)
--            putIOwords ["getMD5 in FileStrings.hs before if"]
            if regular && readable then callIO $ do
    --                    putIOwords ["getMD5 in FileStrings.hs file 1"]
                        filedata :: L.ByteString <- L.readFile fn  -- fails for some special files eg. /proc
    --                    putIOwords ["getMD5 in FileStrings.hs file 2"]
                        let res = showT $ md5  filedata
    --                    putIOwords ["getMD5 in FileStrings.hs file 3"]
                        return $!! (Just res)
--                   `catch` \(e::SomeException) -> do
--                            putIOwords ["caught with catch in getmd5 ", showT e]
--                            return Nothing

                else throwErrorT $ ["getMD5 error file not readable" , showT fn]

        `catchError` \e -> do
            putIOwords ["getMD5 in FileStrings.hs", showT fn, showT e]  -- reached
            throwErrorT $ ["getMD5 error for" , showT fn]

    getDirCont fn  = do
--        putIOwords ["getDirCont", show f]
        testDir <- doesDirExist' fn
        readExec <- getFileAccess fn (True, False, True)
        if testDir && readExec then
            do
               r <- callIO . D.listDirectory $ fn
               let r2 = filter ( \file' -> (file' /= "." && file' /= "..")  ) r
               let r3 = map (fn </>) r2
--               putIOwords ["FileStrigs - getDirCont", showT fn, "files: ", unwordsT . map showT $ r]
               return r3
          else
                throwErrorT
                    ["getDirCont not exist or not readable"
                    , showT fn, showT testDir, showT readExec]

    getDirContentNonHidden fp = do
--        putIOwords ["getDirContentNonHidden", s2t fp]
        r <- getDirCont fp
        let r2 = filter (not . isHidden) r
----        r2 <- callIO $ D.listDirectory (unL fp)
----          would be possible but filter here is simpler
----        let r2 = filter ( \file' -> (file' /= "." && file' /= "..")  ) r
----        let r2 = filter (not . isPrefixOf "." ) r
----        putIOwords ["nonhidden files", show r2]
        return r2
            where
                isHidden = isPrefixOf "."

    deleteFile f = do
         putIOwords ["delete file ", showT f]
         callIO . D.removeFile   $ f



    getAppConfigDirectory = error "not implemented" -- do
--        let prefsfileDir = ".config"
--        homeDir <- callIO $ D.getHomeDirectory
--        return  (homeDir </> prefsfileDir)

--    getSymbolicLinkStatus :: fp ->   ErrIO (Maybe P.FileStatus)
    getSymbolicLinkStatus fp = do
        --    putIOwords ["fileio getSymbolicLinkStatus", fp]
            st <- callIO $ Posix.getSymbolicLinkStatus fp
        --    putIOwords ["fileio getSymbolicLinkStatus done", fp]
            return  $ st
--          `catchError` (\s -> do
--                    putIOwords ["fileio getSymbolicLinkStatus not found", showT fp]
--                    return Nothing)

    -- ^ get status if exist (else Nothing)
    --   is the status of the link, does not follow the link

    getFileAccess fp (r,w,e) = callIO $
         Posix.fileAccess fp r w  e
--              `catchError` \e -> do
--                     putIOwords ["getFileAccess error", showT fp, s2t $ show e]
--                     return False

        -- fails if broken symbolic link?

--    isFileAbeforeB fpa fpb = do
--        statA :: P.FileStatus <- getFileStatus' fpa
--        statB <- getFileStatus' fpb
--        let
--            timea = getModificationTimeFromStatus statA
--            timeb = getModificationTimeFromStatus statB
--        return $ timea < timeb

    getFileModificationTime  fp = do
        stat :: Posix.FileStatus <- getFileStatus' fp
        let
            time = getModificationTimeFromStatus stat
        return time



    openFile2handle fp mode = do
            callIO $ SIO.openFile fp mode
--        `catchError` \e -> do
        -- most likely the dir does not exist.
        -- try to create the file?
--    closeFile _ handle = callIO $ SIO.hClose handle

--unL = t2s . filepath2text lpX
--mkL = mkFilepath lpX . s2t

instance FileSystemOps (Path ar df) where
    getPermissions' = PathIO.getPermissions . unPath
    checkSymbolicLink  fp =   callIO $ D.pathIsSymbolicLink (unL fp)

instance DirOps (Path ar Dir)  where
    doesDirExist' =  PathIO.doesDirExist .unPath
--    getDirPermissions = P.getPermissions
    createDir'  = PathIO.createDir . unPath
--        do
--        t <- doesFileOrDirExist fp
--        if not t then  callIO $ D.createDirectory . unL $ fp
--            else throwErrorT
--                ["File or Dir exists", showT fp]
    renameDir' old new = do  -- :: fp -> fp ->  ErrIO Text
    -- ^ rename directory old to new
        PathIO.renameDir (unPath old) (unPath new)
--        return $ unwordsT
--                        [ "renamed dir from ", showT old
--                            , " to " , showT  new]


    createDirIfMissing' = PathIO.createDirIfMissing True . unPath

    copyDirRecursive old new = PathIO.copyDirRecur (unPath old) (unPath new)

    deleteDirRecursive f = deleteDirRecursive (unL f)

instance (Show (Path ar File)) => FileOps (Path ar File)  where
    doesFileExist'   =  PathIO.doesFileExist . unPath
--    getPermissions' = P.getPermissions
    copyOneFile old new =  copyOneFile (unL old) (unL new)
    renameOneFile old new = do  -- :: fp -> fp ->  ErrIO Text
    -- ^ rename directory old to new
        PathIO.renameFile (unPath old) (unPath new)

    deleteFile f =  deleteFile (unL f)

    getMD5 fp = getMD5 (unL fp)
-- use listDir which separats result in dir and file list and does not include . and ..
--    getDirCont fn  = do
--          r1 <- getDirCont  . unL $ fn
--          let r2 = map mkL r1
--          return r2
--
--    getDirContentNonHidden fp = do
----        putIOwords ["getDirContentNonHidden", unL fp]
--        r <- getDirCont . unL $ fp
--        let r2 = filter (not . isHidden) r
----        r2 <- callIO $ D.listDirectory (unL fp)
----          would be possible but filter here is simpler
----        let r2 = filter ( \file' -> (file' /= "." && file' /= "..")  ) r
----        let r2 = filter (not . isPrefixOf "." ) r
----        putIOwords ["nonhidden files", show r2]
--        let r3 = (map (makeLegalPath . s2t) r2)
--        return (catMaybes r3)

--    isFileAbeforeB fpa fpb = do
--        statA :: P.FileStatus <- getFileStatus fpa
--        statB <- getFileStatus fpb
--        let
--            timea = getModificationTimeFromStatus statA
--            timeb = getModificationTimeFromStatus statB
--        return $ timea < timeb
    getFileModificationTime  fp =  getFileModificationTime (unL fp)


    openFile2handle fp mode =  openFile2handle (unL fp) mode
--    closeFile fp handle = closeFile (unL fp) handle

--    checkSymbolicLink fp =   callIO $ D.pathIsSymbolicLink (unL fp)

    getFileAccess fp (r,w,e) =
        do
--            putIOwords ["getFileAccess", show fp]
            callIO $
                (do

                    Posix.fileAccess (unL fp) r w  e
              `catchError` \e -> do
                     putIOwords ["getFileAccess error", showT fp, s2t $ show e]
                     return False )


unL = FN.toFilePath


readFileT :: Path ar File  -> ErrIO Text
readFileT fp = callIO .  T.readFile . unL $ fp

writeFileT :: Path ar File  -> Text -> ErrIO ()
writeFileT  fp st = callIO $  T.writeFile (unL fp) st
-- attention - does not create file if not existing

instance  (Show (Path ar File)) => FileOps2 (Path ar File) String where

    readFile2 fp = callIO $ readFile   (unL fp)
    -- a strict read (does cloes?)
    writeFile2  fp st = callIO $ writeFile   (unL  fp) st
    appendFile2  fp st = callIO  $   appendFile  (unL fp) st


instance (Show (Path ar File)) =>  FileOps2 (Path ar File) Text where

    readFile2 fp = readFile2 (unL fp)
    -- a strict read (does cloes?)
    -- deal with latin encoded files - gives hGet error otherwise!
    writeFile2  fp st = writeFile2 (unL fp) st
    appendFile2  fp st = appendFile2  (unL fp) st

    writeFileOrCreate2 filepath st = do
        let dir = getParentDir filepath
--        let (dir, fn, ext) = splitFilepath filepath
        --        writeFileCreateDir dir fn st
        --                  writeFileCreateDir dirpath filename st = do
--        let fp = dirpath `combine` filename
--        d <- doesDirExist' dir
--        --        f <- doesFileExist fp --
--        unless d $
        createDirIfMissing' dir
        when False $ putIOwords ["writeFileOrCreate2 dir created", showT dir]
        t <- doesDirExist' dir
        when False $ putIOwords ["writeFileOrCreate2 dir test", showT t]
        writeFile2 filepath st
        when False $ putIOwords ["writeFileOrCreate2 file written", showT filepath]

instance FileOps2 FilePath Text where

    readFile2 fp = callIO $  T.readFile fp
    writeFile2  fp st = callIO $  T.writeFile fp st
    appendFile2  fp st = callIO  $  T.appendFile  fp st

instance FileOps2 FilePath L.ByteString where

    readFile2 fp = callIO $  L.readFile fp
    writeFile2  fp st = callIO $  L.writeFile fp st
    appendFile2  fp st = callIO  $  L.appendFile  fp st

instance (Show (Path ar File)) => FileOps2 (Path ar File) L.ByteString where

    readFile2 fp = callIO $  L.readFile . unL $ fp
    writeFile2  fp st = callIO $  L.writeFile (unL fp) st
    appendFile2  fp st = callIO  $  L.appendFile  (unL fp) st


--bracket2
--        :: IO a         -- ^ computation to run first (\"acquire resource\")
--        -> (a -> IO b)  -- ^ computation to run last (\"release resource\")
--        -> (a -> IO c)  -- ^ computation to run in-between
--        -> IO c         -- returns the value from the in-between computation
--bracket before after thing =
--  Catch.mask $ \restore -> do
--    a <- before
--    r <- restore (thing a) `onException` after a
--    _ <- after a
--    return r
