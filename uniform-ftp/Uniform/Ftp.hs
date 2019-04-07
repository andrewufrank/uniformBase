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

            -- must be changed to statemonad

getConnection :: FTPclient -> FTPConnection 
-- get the connection from the ftp client - must be connected before
getConnection f = fromJustNote "getConnection 42345dd" $ hand f

ftp0 = FTPclient "cp140.sp-server.net" username keycpanel Nothing Nothing

-- changeDir ::Path Abs Dir -> FTPclient -> StateT  FTPclient ErrIO [Text]
-- changeDir dir h  = do 
--         h1 <- connect' h
--         callIO $ cwd (fromJustNote "cwd w4234" $ hand h1) (toFilePath dir)
--         d :: [String] <- callIO $ dir (fromJustNote "cwd w55324" $ hand h1) Nothing 
--         putIOwords ["dir", unlines'. map s2t $ d]
--         return $  map s2t  d 

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

ftpChangeDir :: Path b Dir ->FTPstate ()
ftpChangeDir path = do 
    h <- ftpConnect
    callIO $ cwd h (toFilePath path) 
    return () 

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
    
        -- transferDir :: Path Abs Dir -> ErrIO 


        -- l <- nlst h Nothing 
        -- putIOwords ["nlst", showT  l] 
        -- -- b <- getbinary h "1KB.zip"
        -- -- putIOwords ["getbinary", showT b]
        -- -- d :: [String] <- dir h Nothing 
        -- -- putIOwords ["dir", unlines'. map s2t $ d]

        -- -- r <- uploadbinary h "Setup.lhs"
        -- -- putIOwords ["upload result", showT r]  -- not permitted?
        -- putIOwords ["push2 ends"]
        -- return ()

push2 :: FTPConnection -> IO [Text]
push2  h = do 
    putIOwords ["push2 starts"]
    -- enableFTPDebugging
    -- h <- easyConnectFTP "speedtest.tele2.net" -- "ftp.kernel.org"
    -- loginAnon h
    l <- nlst h Nothing 
    putIOwords ["nlst", showT  l] 
    b <- getbinary h "1KB.zip"
    putIOwords ["getbinary", showT b]
    d :: [String] <- dir h Nothing 
    putIOwords ["dir", unlines'. map s2t $ d]
    return (map s2t d)
    -- r <- uploadbinary h "Setup.lhs"
    -- putIOwords ["upload result", showT r]  -- not permitted?
    -- putIOwords ["push2 ends"]
    -- return ()
     
-- push1 :: IO () 
-- push1 =  do
--         h <- withFTP  "ftp://speedtest.tele2.net" 21 $ op1 
--         -- w <- nlst h []
--         -- print w 
--         return () 

-- op1 h welcome = do
--     print welcome
--     login h "anonymous" "anonymous"
--     w <- nlst h []
--     print w 
