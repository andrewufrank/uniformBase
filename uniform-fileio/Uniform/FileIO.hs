-----------------------------------------------------------------------------
--
-- Module      :  FileIO
-- Copyright   :  andrew u frank -
--
-- | the basic file io - translated for the Either or ErrorT signaling style
--  there are better (higher performance methods - replace while retaining conditions

-- uses the Path and Path.IO framework

-- this is the general export
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.FileIO (
        module Uniform.Filenames
--         , module Uniform.Error
        --  , module Uniform.Strings
         , module Uniform.FileStatus
         , module Uniform.FileIOalgebra
         , module Uniform.TypedFile
         , module Uniform.FileStrings
         , module Uniform.Piped
            ,  getAppUserDataDir'
         , makeAbsoluteFile'
         , findDirs, isDir 
--         , homeDir2
            ) where

import qualified System.Posix          as P (FileStatus)
import           Uniform.FileIOalgebra hiding ((<.>), (</>))
import           Uniform.Filenames
import           Uniform.FileStatus
import           Uniform.FileStrings
import           Uniform.Piped
import           Uniform.TypedFile
import           Uniform.Zero
import qualified Path.IO (makeAbsolute, getAppUserDataDir)

makeAbsoluteFile' :: Path a File -> ErrIO (Path Abs File)
makeAbsoluteFile' file = do
            f2 <- Path.IO.makeAbsolute (unPath file)
            return  f2

getAppUserDataDir' :: String -> ErrIO (Path Abs Dir)
getAppUserDataDir' appName  = Path.IO.getAppUserDataDir $ appName

findDirs :: [FilePath] -> ErrIO [Path Abs Dir]
-- ^ find directories in a list of files
findDirs fns = do
    mdirs <- mapM isDir fns
    return . catMaybes $ mdirs

isDir :: FilePath -> ErrIO (Maybe (Path Abs Dir))
-- ^ is this filepateh an directory 
isDir fn = do
    st <- getFileStatus' fn
    return (if isDirectory st then Just (makeAbsDir fn) else Nothing)

