-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Example
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
{-# LANGUAGE DeriveGeneric  #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}


 
module Uniform.PathWrapper
    (module Uniform.PathWrapper
    , P.File, P.Dir, P.Abs, P.Rel
    )

where

import Uniform.Strings
import Uniform.Error

import qualified Path as P
import Path (File, Dir, Abs, Rel)
-- import Data.Typeable
-- import Data.Data 


data Path a d = Path  Text
    deriving (Show, Read, Ord, Eq, Generic)

class PathMake path ppath a d where 
    path2internal :: path a d -> ppath a d 
instance PathMake Path P.Path Rel File where
    path2internal (Path  fn) = 
            fromJustNote ("makeRelFile " <> fp) $ P.parseRelFile fp
            where fp = t2s fn
instance PathMake Path P.Path Rel Dir where
    path2internal (Path  fn) = 
            fromJustNote ("makeRelDir " <> fp) $ P.parseRelDir fp
            where fp = t2s fn

makeRelFile :: Text -> Path Rel File 
makeRelFile t1 = Path . toFilePath2 . fromJustNote "makeRelFile" $ f1  
    where 
        f1 = P.parseRelFile . t2s $ t1  :: Maybe (P.Path Rel File)
makeRelDir :: Text -> Path Rel Dir 
makeRelDir t1 = Path . toFilePath2 . fromJustNote "makeRelDir" $ f1  
    where 
        f1 = P.parseRelDir . t2s $ t1  :: Maybe (P.Path Rel Dir)

toFilePath2 = s2t . P.toFilePath 