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
    -- , AR (..), DF (..)
    )

where

import Uniform.Strings
import Uniform.Strings.Infix
import Uniform.Error

import qualified Path as P
import qualified Path as Path
import Path (File, Dir, Abs, Rel)

import qualified System.FilePath               as S
import qualified System.FilePath.Posix         as S -- prefered


data AR = Rel | Abs   deriving (Show, Read, Ord, Eq, Generic)
data DF = File | Dir  deriving (Show, Read, Ord, Eq, Generic)

data Path a d = Path AR DF Text
    deriving (Show, Read, Ord, Eq, Generic)
toFilePath  (Path _ _ t) = t2s t   
toFilePathT  (Path _ _ t) =   t   
unPath :: (PathMake Path P.Path a d) => Path a d -> P.Path a d 
unPath = path2internal 

class PathMake1 p where
    makePath :: FilePath -> p
instance PathMake1 (Path Rel Dir) where 
    makePath = makeRelDir . s2t
instance PathMake1 (Path Rel File) where 
    makePath = makeRelFile . s2t

class PathMake path ppath a d where 
    path2internal :: path a d -> ppath a d 
    internal2path :: ppath a d -> path a d
instance PathMake Path P.Path Rel File where
    path2internal (Path _ _ fn) = 
            fromJustNote ("makeRelFile " <> fp) $ P.parseRelFile fp
            where fp = t2s fn
    internal2path pp = makeRelFile . toFilePath2 $ pp

instance PathMake Path P.Path Rel Dir where
    path2internal (Path _ _ fn) = 
            fromJustNote ("makeRelDir " <> fp) $ P.parseRelDir fp
            where fp = t2s fn

instance PathMake Path P.Path Abs File where
    path2internal (Path _ _ fn) = 
            fromJustNote ("makeAbsFile " <> fp) $ P.parseAbsFile fp
            where fp = t2s fn
instance PathMake Path P.Path Abs Dir where
    path2internal (Path _ _ fn) = 
            fromJustNote ("makeAbsDir " <> fp) $ P.parseAbsDir fp
            where fp = t2s fn


makeRelFile :: Text -> Path Rel File 
makeRelFile t1 = Path Rel File . toFilePath2 . fromJustNote "makeRelFile" $ f1  
    where 
        f1 = P.parseRelFile . t2s $ t1  :: Maybe (P.Path Rel File)
makeRelDir :: Text -> Path Rel Dir 
makeRelDir t1 = Path Rel Dir . toFilePath2 . fromJustNote "makeRelDir" $ f1  
    where 
        f1 = P.parseRelDir . t2s $ t1  :: Maybe (P.Path Rel Dir)
makeAbsFile :: Text -> Path Abs File 
makeAbsFile t1 = Path Abs File . toFilePath2 . fromJustNote "makeAbsFile" $ f1  
    where 
        f1 = P.parseAbsFile . t2s $ t1  :: Maybe (P.Path Abs File)
makeAbsDir :: Text -> Path Abs Dir 
makeAbsDir t1 = Path Abs Dir . toFilePath2 . fromJustNote "makeAbsDir" $ f1  
    where 
        f1 = P.parseAbsDir . t2s $ t1  :: Maybe (P.Path Abs Dir)

toFilePath2 = s2t . P.toFilePath 

--- experiments with some ops 
class Filenames fp fr where
    getFileName :: fp -> fr

class Filenames4 fp file  where
    type FileResultT4 fp file
    -- add a filepath to a absolute dir and givev an absolte dir
    --
    addDir  :: fp -> file -> FileResultT4 fp file

instance Filenames FilePath FilePath where
    getFileName = snd . S.splitFileName

instance Filenames (Path ar File) (Path Rel File) where
    -- getFileName = makeRelFile . toFilePath2 . Path.filename . unPath
    getFileName = makeRelFile . s2t .  getFileName . toFilePath 

instance Filenames4 FilePath FilePath  where
    type FileResultT4 FilePath FilePath = FilePath
    addDir p d = if null' d then p else p S.</> d

instance (PathMake1 (Path b Dir)) => Filenames4 (Path b Dir) FilePath  where
    type FileResultT4 (Path b Dir) FilePath = (Path b Dir)
    addDir p d = makePath $ addDir (toFilePath p) d
        -- then  p
        -- else p </> d2
        -- where d2 = makeRelDir d :: Path Rel Dir
instance (PathMake1 (Path b t)) => Filenames4 (Path b Dir) (Path Rel t)  where
    type FileResultT4 (Path b Dir) (Path Rel t) = (Path b t)
    addDir p d = makePath $ addDir (toFilePath p) (toFilePath d)
            -- (Path.</>) (unPath p) (unPath d)
