-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Webserver
--
-- | a miniaml set of
-----------------------------------------------------------------------------
-- {-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.WebServer (module Uniform.WebServer
        )  where

import           Uniform.Strings hiding ((</>), (<.>), S)
import           Web.Scotty hiding (File)
import           Network.Wai.Middleware.Static  ( staticPolicy
                                                , addBase
                                                )
import           Network.Wai.Handler.Warp       ( Port ) -- .Warp.Types
import Uniform.FileIO

site :: Path Abs Dir -> Path Rel File -> ScottyM ()
-- for get, return the page from baked
-- for post return error
-- the bakedPath is the origin for the relative url
-- the landing is the rel page name for the landing page 
site bakedPath landing = do
    get "/" $ file (toFilePath $ landingPage landing bakedPath)
    middleware $ staticPolicy $ addBase (toFilePath bakedPath)

landingPage :: Path Rel File -> Path Abs Dir -> Path Abs File 
landingPage landingFn bakedPath =
     addFileName bakedPath landingFn 
            -- (makeRelFile "landingPage.html")


