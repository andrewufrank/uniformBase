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
        )  where

import           Uniform.Strings hiding ((</>), (<.>), S)
import "ftphs" Network.FTP.Client
import Uniform.Error 
import Uniform.FileIO 
import Control.Monad.Trans.State 
-- import qualified Control.Monad.HT (zipWith)

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

-- ftpMakeDir :: Path Abs Dir -> FTPstate ()
-- -- check if it exists already - no op if exist
-- ftpMakeDir path = do 
--     (a,s) <- runStateT $ do 
--         h <- ftpConnect
--         (fn, res) <- callIO $ mkdir h (toFilePath path) 
--         putIOwords ["ftpMakeDir", "fn", showT fn, "res", showT res ]
--         return () 
--     return ()
--   `catch` (\e -> do 
--         putIOwords ["ftpMakeDir error", showT e ]
--         -- putIOwords ["ftpMakeDir error", "fn", showT fn, "res", showT res ]
--         )

ftpMakeDir :: Path Abs Dir -> FTPstate ()
-- check if it exists already - no op if exist
ftpMakeDir path = do 
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
-- ignore the dirs in the file 
-- the target dir must exist 
ftpUploadFilesFromDir source target = do 
    h <- ftpConnect 
    files :: [FilePath] <- lift $ getDirContentFiles  (toFilePath source)
    let files2 = map (fromJustNote "stripPrefix ftpUpload 77454" . stripPrefix' (toFilePath source) ) files
    putIOwords ["ftpUpload fils to upload", showT files2]
    mapM  (\s -> do 
                    cont :: Text <- lift $ do 
                                                c <- readFile2 (toFilePath source  </> s)
                                                putIOwords ["ftpUpload read", showT s]
                                                return c
                    liftIO $ putbinary h (toFilePath target </> s) (t2s cont)
                ) files2
    return () 

ftpUploadDirsRecurse :: Path Abs Dir -> Path Abs Dir -> FTPstate ()
-- recursive upload of a dir
ftpUploadDirsRecurse source target = do 
    h <- ftpConnect
    -- make directory and upload files  
    ftpMakeDir target 
    ftpUploadFilesFromDir source target
    -- get all the directories 
    (dirs1, targets) <- lift $ do 
        dirs :: [FilePath] <- getDirectoryDirs' (toFilePath source )
        let dirs1 = map makeAbsDir dirs
        putIOwords ["\n\nftpUploadDirsRecurse dirs ", showT dirs1]
        let targets = map (\f-> target </>  (fromJustNote "234233772" . stripProperPrefixM source $ f)) dirs1
        putIOwords ["ftpUploadDirsRecurse targets ", showT targets]
        return (dirs1,targets)
    -- create target dirs 

    -- let 
    let sts = zip dirs1 targets :: [(Path Abs Dir, Path Abs Dir)]
    putIOwords ["ftpUploadDirsRecurse recurse  ", "dirs", showT dirs1, "targets", showT targets]
    [rs] :: [ ()]<- mapM (\(s,t) -> ftpUploadDirsRecurse s t)  sts
    putIOwords ["ftpUploadDirsRecurse end "]

ftpUpload2 :: Path Abs Dir -> Path Abs Dir -> FTPstate ()
-- upload all the files in a directory 
-- ignore the dirs in the file 
-- create targetDir 
ftpUpload2 source target = do 
    putIOwords ["ftpUpload2 ", showT source, showT target]
    ftpMakeDir target 
    putIOwords ["ftpUpload2 ", "created" , showT target]


-- x :: m(a->b->c) -> [a] -> [b] -> m [c]