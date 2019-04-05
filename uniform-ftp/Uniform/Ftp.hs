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

push2 :: IO ()
push2 = do 
        putIOwords ["push2 starts"]
        putIOwords ["push2 ends"]
        return ()
        
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
