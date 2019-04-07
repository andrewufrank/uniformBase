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
{-# LANGUAGE RecordWildCards  #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.Ftp (module Uniform.Ftp
        )  where

import           Uniform.Strings hiding ((</>), (<.>), S)
import "ftphs" Network.FTP.Client
import Uniform.Error 
import Uniform.FileIO 
import Control.Monad.Trans.State 

username = "gerastre"
keycpanel = "geras125cpanel"

data FTPclient = FTPclient 
            { v1, v2, v3 :: Text 
            , curDir :: Maybe (Path Abs Dir)
            , hand ::Maybe FTPConnection -- connected and login
            } -- no show for FTPConnection -- deriving (Read, Show, Ord, Eq)

getConnection :: FTPclient -> FTPConnection 
-- get the connection from the ftp client - must be connected before
getConnection f = fromJustNote "getConnection 42345dd" $ hand f

ftp0 = FTPclient "cp140.sp-server.net" username keycpanel Nothing Nothing


type FTPstate =  StateT  FTPclient ErrIO 

ftpConnect ::  FTPstate FTPConnection -- 
        -- StateT  FTPclient ErrIO FTPConnection  
-- establish the connection, idempotent 
ftpConnect  = do
        ftp <- get 
        putIOwords ["connect starts"]
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

ftpChangeDir :: Path b Dir -> FTPstate ()
ftpChangeDir path = do 
    h <- ftpConnect
    callIO $ cwd h (toFilePath path) 
    return () 

ftpMakeDir :: Path Abs Dir -> FTPstate ()
-- check if it exists already 
ftpMakeDir path = do 
    h <- ftpConnect
    callIO $ mkdir h (toFilePath path) 
    return () 

ftpCurrentDir ::  FTPstate (Path Abs Dir)
ftpCurrentDir  = do 
    h <- ftpConnect
    (p,e) <- callIO $ pwd h 
    return . makeAbsDir . fromJustNote "ftpCurrent dir werw222" $ p 

ftpDir :: FTPstate [Text]
ftpDir = do 
    h <- ftpConnect 
    s <- callIO $ nlst h Nothing 
    return (map s2t s)

ftpUpload2currentDir :: Path Abs File -> Path Rel File -> FTPstate () 
-- upload a file relative to current dir 
ftpUpload2currentDir source target = do 
    h <- ftpConnect 
    content :: String <- lift $ readFile2 source
    callIO $ do 
            res <- putbinary h (toFilePath target) content 
            putIOwords ["ftpUpload", showT source, showT target, showT res]

    return () 
    
ftpUpload  :: Path Abs File -> Path Abs File -> FTPstate () 
-- upload a file to absolute path (relative to root)
ftpUpload  source target = do 
    h <- ftpConnect 
    content :: String <- lift $ readFile2 source
    callIO $ do 
            res <- putbinary h (toFilePath target) content 
            putIOwords ["ftpUpload", showT source, showT target, showT res]

    return () 
    
ftpUploadFilesFromDir :: Path Abs Dir -> Path Abs Dir -> FTPstate ()
-- upload all the files in a directory 
-- ignore the dirs
ftpUploadFilesFromDir source target = do 
    h <- ftpConnect 
    files :: [FilePath] <- lift $ getDirContentFiles 
            (toFilePath source)
    mapM  (\s -> do 
                    cont :: Text <- lift $ readFile2 s
                    liftIO $ putbinary h (toFilePath target </> s) (t2s cont)
                ) files
    return () 
