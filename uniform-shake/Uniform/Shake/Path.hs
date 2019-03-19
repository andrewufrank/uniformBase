{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- {-# OPTIONS -Wall #-}
--{-# OPTIONS -fno-warn-missing-signatures #-}

module Uniform.Shake.Path
    ( getHashedShakeVersionP
    , getDirectoryFilesP
    , needP
    , wantP
    , ($%>)
    , ($&%>)
    , orderOnlyP
    , cmd
    , liftIO
    , CmdOption(..)
    , module Path
    -- , module Path.IO
    ) where

import Development.Shake
        -- (getDirectoryFiles, Action
        --     , Rules, FilePattern)
import Uniform.FileIO (makeRelFile)

import qualified Path  
import  Path  (Path(..), File, Dir, Abs, Rel, toFilePath)
import qualified Path.IO

getDirectoryFilesP :: Path Abs Dir -> [FilePattern] -> Action [Path Rel File] 
getDirectoryFilesP d p = do 
            res :: [FilePath] <- getDirectoryFiles (toFilePath d) p
            return $ map makeRelFile res

getHashedShakeVersionP :: [Path r File] -> IO String
getHashedShakeVersionP = getHashedShakeVersion . map toFilePath

needP :: [Path r File] -> Action ()
needP = need . map toFilePath

wantP :: [Path r File] -> Rules ()
wantP = want . map toFilePath

($%>) :: Path r File -> Action () -> Rules ()
p $%> a = toFilePath p %> const a

($&%>) :: [Path r File] -> Action () -> Rules ()
ps $&%> a = map toFilePath ps &%> const a

orderOnlyP :: [Path r File] -> Action ()
orderOnlyP = orderOnly . map toFilePath
