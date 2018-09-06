-----------------------------------------------------------------------------
--
-- Module      :  FileIO.ByteString
--
-- | the implementation for filepath encoded as bytestring (RawFilePath = FilePathX)
--
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE
    MultiParamTypeClasses
    , TypeSynonymInstances
--    , FunctionalDependencies
    , FlexibleInstances
    , FlexibleContexts
--    , DeriveFunctor
    , ScopedTypeVariables
    , UndecidableInstances
    , OverloadedStrings
    , TypeFamilies
    #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}


module FileIO.ByteString (
    module FileIO.ByteString
    , module FileIO.FileIO
    , htf_thisModulesTests
) where

-- using uniform
import Uniform.Error
-- import Data.Strings
import FileIO.FileIO

import Test.Framework
import Test.Invariant

import qualified Data.ByteString as BS

import System.Posix.ByteString   -- what would be the windows corresponance? --needs cygwin...
import qualified System.Posix.FilePath as P
import System.Posix.FilePath (RawFilePath)
import qualified Data.Text as T
import qualified Data.Text.IO as T

--import Basics  hiding ((</>), (<.>))
import FileIO.Strings

import qualified System.Directory as S (getHomeDirectory)


doWithString op = s2b . op . b2s

instance   FilePathes0 RawFilePath  where
instance (CharChains RawFilePath)  => FilePathes2 RawFilePath  where

    splitExtension = P.splitExtension
--    addTo f s = f </>  (s2b s)
--    addExt f s = f <.> (s2b s)
--    makeFP = s2b
--    takeExtension1 f = b2s .  P.takeExtension $ f
--    takeExtension2 f = tailNote "takeExtension2" . b2s .  P.takeExtension $ f
----    replaceExtension f e = s2b  (replaceExtension (  b2s $ f)  e)
    addExtension0   =   P.addExtension

--test_addExt = assertEqual "" (P.addExtension "\3152" "\426")
--
--prop_FP_fp2 :: Text -> Text -> Bool
---- i have no arbitrary for bytestring
---- tests fails, for input like above
--prop_FP_fp2 f e = prop_extension2 (t2b f) (t2b e)

instance FilePathes FilePath => FilePathes RawFilePath where
    combine0  f = doWithString  $ combine0 (b2s f)

    makeRelative f = doWithString (makeRelative $ b2s f)
    isHidden2 =    isHidden2 . b2s
    splitFileName0 = P.splitFileName
--    splitDirFile   =  P.splitPath
    splitDirFiles2 = P.splitDirectories
    getFilepath = b2s
--    takeLastDirName  "" = ""
--    takeLastDirName fp = s2b . last . splitDirFile  . b2s $  fp
--    removeExtension = s2b . remove . b2s

instance (FilePathes String , FileOps FilePath) => FileOps RawFilePath where
    doesDirectoryExist = doesDirectoryExist . b2s
    doesFileExist   = doesFileExist .b2s
    doesFileOrDirExist = doesFileOrDirExist . b2s

    createDir   = createDir . b2s

    createDirIfMissing fp = createDirIfMissing . b2s $ fp


    copyOneFile old new = copyOneFile (b2s old) (b2s new)

    renameOneFile old new = renameFile (b2s old) (b2s new)

    renameDirectory old new = renameDirectory (b2s old) (b2s new)


    getMD5 fn = getMD5 . b2s $ fn

    getDirCont f  = getDirCont . b2s $ f
    getDirContentNonHidden f  = getDirContentNonHidden . b2s $ f

    deleteFile f = deleteFile . b2s $ f
    deleteDirRecursive f = deleteDirRecursive . b2s $ f

    getAppConfigDirectory =
        do
            let prefsfileDir = ".config" :: String
            homeDir <- callIO $ S.getHomeDirectory
            return . s2b $ (homeDir </> prefsfileDir)



instance FileOps2 RawFilePath T.Text where

    readFile2 fp = callIO $  T.readFile  (b2s fp)
    writeFile2  fp st = callIO  $  T.writeFile  (b2s fp) st
    appendFile2  fp st = callIO  $  T.appendFile  (b2s fp) st

instance  FileOps2 RawFilePath BS.ByteString where

    readFile2 fp = callIO $ BS.readFile  (b2s fp)
    writeFile2  fp st = callIO $ BS.writeFile  (b2s fp) st
    appendFile2  fp st = callIO  $  BS.appendFile  (b2s fp) st
