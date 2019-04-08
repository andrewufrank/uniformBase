-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Ftp
--
-- | a miniaml set of
-----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.Ftp (module Uniform.Ftp
        )  where

import           Uniform.Strings hiding ((</>), (<.>), S)
import "ftphs" Network.FTP.Client
import Uniform.Error 
import Uniform.FileIO 

push2 :: IO ()
push2 = do 
        h <- connect "Gerastree" 
        cwd h "ssg.gerastree.at"
        d :: [String] <- dir h Nothing 
        putIOwords ["dir", unlines'. map s2t $ d]

connect :: Text -> IO FTPConnection
connect "Gerastree" = do 
        putIOwords ["push2 starts"]
        enableFTPDebugging
        h <- easyConnectFTP "cp140.sp-server.net" -- there should be a gerastree.address
        login h "gerastre" (Just "geras125cpanel") Nothing  
        return h

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

        -- push2 = do 
        --     putIOwords ["push2 starts"]
        --     enableFTPDebugging
        --     h <- easyConnectFTP "speedtest.tele2.net" -- "ftp.kernel.org"
        --     loginAnon h
        --     l <- nlst h Nothing 
        --     putIOwords ["nlst", showT  l] 
        --     b <- getbinary h "1KB.zip"
        --     putIOwords ["getbinary", showT b]
        --     d :: [String] <- dir h Nothing 
        --     putIOwords ["dir", unlines'. map s2t $ d]
    
        --     r <- uploadbinary h "Setup.lhs"
        --     putIOwords ["upload result", showT r]  -- not permitted?
        --     putIOwords ["push2 ends"]
        --     return ()
     
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
