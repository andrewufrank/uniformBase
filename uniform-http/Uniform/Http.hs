
 -----------------------------------------------------------------------------
--
-- Module      :  Uniform.HttpCall
--
-- | the only externally visible module
-- exports all
-- using http simple to sparql queries and to create requests
-- part of uniform (to use only text
-- uses the newer http-conduit module
-- because teh old HTTP cannot do https

-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DeriveAnyClass,
  RecordWildCards #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Uniform.Http (
      TimeOutSec, mkTimeOutSec, mkTimeOutDefault
    , mkServerURI, mkServerURI4text 
    , ServerURI, addPort2ServerURI
    , PortNumber, mkPortNumber
    , URI, makeURI, addToURI 
    , uriT  -- required?
    , mkAppType, AppType
    , callHTTP10post
    , mkHttpPath, HttpPath   -- from / to ?
    , mkHttpQueryParams, HttpQueryParams  -- after the ?
    , module Uniform.Error
            )  where

import           Uniform.Error
import Uniform.HttpCall
import Uniform.HttpURI



