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
        )  where

import           Uniform.Strings hiding ((</>), (<.>), S)
import "ftphs" Network.FTP.Client
import Uniform.Error 
import Uniform.FileIO 
import Control.Monad.Trans.State 
import Data.List.Utils
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
            -- res <- putbinary h target input
            -- putIOwords ["uploadbinary2", showT source ]
            return  ""

                       
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
    
ftpUploadFilesFromDir :: Path Abs Dir -> Path Abs Dir -> FTPstate ()
-- upload all the files in a directory 
-- ignore the dirs in the file 
-- the target dir must exist 

ftpUploadFilesFromDir source target = do 
    putIOwords ["ftpUploadFilesFromDir", showT source, showT target]
    h <- ftpConnect 
    ftpMakeDir target 

    files :: [Path Abs File] <- lift $ getDirContentNonHiddenFiles  source
    let files2 = map (fromJustNote "stripPrefix ftpUpload 77454" . stripProperPrefixMaybe source) files
            :: [Path Rel File]
    putIOwords ["ftpUploadFilesFromDir files to upload", showT files2]
    mapM_ (\s -> ftpUpload (source </> s) (target </> s)) (seqList files2)

ftpUploadDirsRecurse :: Path Abs Dir -> Path Abs Dir -> FTPstate ()
-- recursive upload of a dir
ftpUploadDirsRecurse source target = do 
    putIOwords ["ftpUploadDirsRecurse", showT source, showT target]
    h <- ftpConnect
    -- make directory and upload files  
    ftpUploadFilesFromDir source target
    -- get all the directories 
    (dirs1, targets) <- lift $ do 
        dirs1 :: [Path Abs Dir] <- getDirectoryDirsNonHidden' source
        putIOwords ["\n\nftpUploadDirsRecurse for ", showT source, " dirs ", showT dirs1]
        let targets = map (\f-> target </>  (fromJustNote "234233772" . stripProperPrefixMaybe source $ f)) dirs1
        putIOwords ["ftpUploadDirsRecurse targets ", showT targets]
        return (dirs1,targets)
 
    let sts = zip dirs1 targets :: [(Path Abs Dir, Path Abs Dir)]

    if not . null $ sts 
        then do 
            putIOwords ["ftpUploadDirsRecurse recurse  ", "dirs and targets", unwords'. map showNice $ sts]
            [rs] :: [ ()]<- mapM (\(s,t) -> ftpUploadDirsRecurse s t) (seqList sts)
            putIOwords ["ftpUploadDirsRecurse end "]
        else do 
            putIOwords ["ftpUploadDirsRecurse no recursion", showT sts, "for source", showT source]
            return ()
