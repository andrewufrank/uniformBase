-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Ftp
--
-- | a miniaml set of
-----------------------------------------------------------------------------
-- {-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
-- {-# LANGUAGE RecordWildCards  #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.Ftp (module Uniform.Ftp
    , runStateT 
    , testNewerModTime
        )  where

import           Uniform.Strings hiding ((</>), (<.>), S)
import "ftphs" Network.FTP.Client
import Uniform.Error 
import Uniform.FileIO 
import Uniform.Time
import Control.Monad.Trans.State 
import Data.List.Utils -- from MissingH
import System.IO.Binary (readBinaryFile)
username = "gerastre"
keycpanel = "geras125cpanel"

data FTPclient = FTPclient 
            { v1, v2, v3 :: Text 
            -- , curDir :: Maybe (Path Abs Dir)  -- not used
            , hand ::Maybe FTPConnection -- connected and login
            } -- no show for FTPConnection -- deriving (Read, Show, Ord, Eq)

getConnection :: FTPclient -> FTPConnection 
-- get the connection from the ftp client - must be connected before
getConnection f = fromJustNote "getConnection 42345dd" $ hand f

ftp0 = FTPclient "cp140.sp-server.net" username keycpanel Nothing 


type FTPstate =  StateT  FTPclient ErrIO 

ftpConnect ::  FTPstate FTPConnection -- 
        -- StateT  FTPclient ErrIO FTPConnection  
-- establish the connection, idempotent 
ftpConnect  = do
        ftp <- get 
        -- putIOwords ["connect starts"]
        case hand ftp of 
            Nothing -> do 
                h <- callIO $ do 
                    enableFTPDebugging
                    h <- easyConnectFTP (t2s $ v1 ftp) -- there should be a gerastree.address
                    login h (t2s $ v2 ftp) (Just . t2s $ v3 ftp) Nothing
                    return h  
                put (ftp{hand = Just h})
                return h
            Just _ -> return (getConnection ftp)

ftpChangeDir :: Show (Path b Dir) => Path b Dir -> FTPstate ()
ftpChangeDir path = do 
    putIOwords ["connect ftpChangeDir", showT path]
    h <- ftpConnect
    callIO $ cwd h (toFilePath path) 
    return () 


ftpMakeDir :: Path Abs Dir -> FTPstate ()
-- check if it exists already - no op if exist
ftpMakeDir path = do 
        putIOwords ["ftpMakeDir", showT path]
        h <- ftpConnect
        callIO $ do 
                         mkdir h (toFilePath path) 
                         return () 
                    `catchError` (\e   -> do 
                        putIOwords ["ftpMakeDir error", showT e ]
                        if "user error (FTP:550" `isPrefixOf'` showT e then return () 
                                else throwError e 
                                    -- putIOwords ["ftpMakeDir error", "fn", showT fn, "res", showT res ]
                                    )
        -- putIOwords ["ftpMakeDir", "fn", showT fn, "res", showT res ]
        return () 
 
ftpCurrentDir ::  FTPstate (Path Abs Dir)
-- get current dir 
ftpCurrentDir  = do 
    putIOwords ["ftpCurrentDir"]
    h <- ftpConnect
    (p,e) <- callIO $ pwd h 
    return . makeAbsDir . fromJustNote "ftpCurrent dir werw222" $ p 

ftpDir :: FTPstate [Text]
ftpDir = do 
    putIOwords ["ftpDir"]
    h <- ftpConnect 
    s <- callIO $ nlst h Nothing 
    return (map s2t s)

ftpUpload2currentDir :: Path Abs File -> Path Rel File -> FTPstate () 
-- upload a file relative to current dir 
ftpUpload2currentDir source target = do 
    putIOwords ["ftpUpload2currentDir", showT source, showT target]
    h <- ftpConnect 
    -- content :: String <- lift $ readFile2 source
    callIO $ do 
            -- res <- putbinary h (toFilePath target) content 
            res <- uploadbinary2 h (toFilePath source) (toFilePath target)
            putIOwords ["ftpUpload2currentDir files to upload"
                        , showT source, showT target, showT res]

    return () 
{- | Uploads a file from disk in binary mode. Note: filename is used for both local and remote. -}
uploadbinary2 :: FTPConnection -> FilePath -> FilePath  -> IO Text
uploadbinary2 h source target = do 
            input <- readBinaryFile source
            res <- -- return "" 
                     putbinary h target input
            putIOwords ["uploadbinary2 ", showT source ,"----------------------FTP transfer"]
            return  . showT $ res 

                       
ftpUpload  :: Path Abs File -> Path Abs File -> FTPstate () 
-- upload a file to absolute path (relative to root)
ftpUpload  source target = do 
    putIOwords ["ftpUpload", showT source, showT target]
    h <- ftpConnect 
    -- content :: String <- lift $ readFile2 source
    callIO $ do 
            res <- uploadbinary2 h (toFilePath source) (toFilePath target) 
            putIOwords ["ftpUpload", showT source, showT target, showT res]

    return () 
    
ftpUploadFilesFromDir ::(Path Abs File -> ErrIO Bool) -> Path Abs Dir -> Path Abs Dir -> FTPstate ()
-- upload all the files in a directory 
-- ignore the dirs in the file 
-- the target dir must exist 

ftpUploadFilesFromDir test source target = do 
    putIOwords ["ftpUploadFilesFromDir", showT source, showT target]
    h <- ftpConnect 
    ftpMakeDir target 

    files1 :: [Path Abs File] <- lift $ getDirContentNonHiddenFiles  source
    files2 <-lift $ filterM test files1
    let files3 = map (fromJustNote "stripPrefix ftpUpload 77454" . stripProperPrefixMaybe source) 
                files2 :: [Path Rel File]
    putIOwords ["ftpUploadFilesFromDir files to upload", showT files3]
    mapM_ (\s -> ftpUpload (source </> s) (target </> s)) (seqList files3)

ftpUploadDirsRecurse :: (Path Abs File -> ErrIO Bool) ->  Path Abs Dir -> Path Abs Dir -> FTPstate ()
-- recursive upload of a dir
ftpUploadDirsRecurse test source target = do 
    putIOwords ["ftpUploadDirsRecurse for source", showT source, showT target]
    h <- ftpConnect
    -- make directory and upload files  
    ftpUploadFilesFromDir test source target
    -- get all the directories 
    
    dirs1 :: [Path Abs Dir] <- lift $ getDirectoryDirsNonHidden' source
    if not . null $ dirs1 
        then do 
            putIOwords ["\n\nftpUploadDirsRecurse ", " dirs ", showT dirs1]
            let targets = map (\f-> target </>  (fromJustNote "234233772" . stripProperPrefixMaybe source $ f)) dirs1
            putIOwords ["ftpUploadDirsRecurse targets ", showT targets]
            -- return (dirs1,targets)

            let sts = zip dirs1 targets :: [(Path Abs Dir, Path Abs Dir)]

            putIOwords ["ftpUploadDirsRecurse recurse  ", "dirs and targets", unwords'. map showNice $ sts]
            mapM_ (\(s,t) -> ftpUploadDirsRecurse test s t) (seqList sts)
            putIOwords ["ftpUploadDirsRecurse end "]
        else do 
            putIOwords ["ftpUploadDirsRecurse no recursion", showT dirs1, "for source", showT source]
            return ()



testNewerModTime :: UTCTime -> Path Abs File -> ErrIO Bool 
testNewerModTime basetime f = do 
        etime <- getFileModificationTime f
        let eutc = fromEpochTime' etime
        let b = basetime < eutc
        when b $ putIOwords ["testNewerModTime basetime ", showT basetime
                    , "is older than", showT eutc]
        return b 