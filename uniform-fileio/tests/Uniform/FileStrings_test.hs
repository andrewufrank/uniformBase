------------------------------------------------------------------------------
--
-- Module      :  FileIO.Strings
--
-- | the instance for strings (was in 0.1.1)
-- filenames are Path
-- should only export the instances
-- removed -- file content can be lazy bytestring
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS -w #-}

module Uniform.FileStrings_test  where

import Uniform.FileStrings
import           Uniform.FileIOalgebra
--import           Uniform.FilenamesAlgebra
import           Uniform.Filenames
import           Uniform.FileStatus
-- import           Uniform.Strings hiding ((<.>), (</>))

import           Test.Framework
import           Test.Invariant

--import           Path                   as P
--import           Path.IO                as P
--
--import           Path
--import           Path.IO
--
---- what is further required?
--import qualified System.IO              as SIO
--import           System.Posix           (FileMode)
--
--
--import qualified Data.ByteString        as BS (readFile, writeFile)
import qualified Data.ByteString.Lazy   as L
--import           Data.Digest.Pure.MD5   (md5)
----import Data.Hash.MD5 (md5s)
--import           Data.Maybe             (catMaybes)
--import qualified Data.Text.IO           as T (readFile, writeFile, appendFile)
--import qualified Data.Text.IO           as TIO (hGetLine, hPutStr)
--
--import qualified System.Directory       as D
----import qualified System.Directory      as D
--import qualified System.FilePath        as OS
----       (addExtension, makeRelative, FilePath, combine, splitPath,
----        takeDirectory, replaceExtension, takeExtension)
--import qualified System.Posix           as P
---- for fileAccess
--import           Control.Arrow          (second)
--import           Control.DeepSeq        (force, ($!!))
--import           Control.Exception      (SomeException, catch)
--import           Control.Monad.Catch
--import           Control.Monad.IO.Class
import           Data.Either            (isLeft)
--import           Data.List              (isPrefixOf)

----for testing:
--readFile5 :: Path ar File -> IO Text
--readFile5 = fmap s2t .readFile . toFilePath


--------------------------test with path

notexisting = makeRelFile "xxxxabcd"

test_catch_error2p = do
    res <- runErr $ do
                            f :: Text <-  readFile2 notexisting
                            return False
                `catchError `
                            \(e::Text) ->  return True
    assertEqual (Right True) res

test_call_IOp = do
    res <- runErr $ do
        f :: String <-   readFile2 notexisting  -- not existing fileAccess
        return False   -- expect that read fials
    assertEqual ( Left "xxxxabcd: openFile: does not exist (No such file or directory)") res

test_call_IO_Lp = do
    res <- runErr $ do
        f :: L.ByteString <-  readFile2 notexisting  -- not existing fileAccess
        return False   -- expect that read fials
    assertEqual ( Left "xxxxabcd: openBinaryFile: does not exist (No such file or directory)") res

test_call_NotExist = do
    res <- runErr $ do
        f :: Bool <-  doesFileExist' notexisting  -- not existing fileAccess
        return f
    assertEqual  (Right False) res

--test_call_IO_Corrupt= do
--    res <- runErr $ callIO $ do
--        f :: L.ByteString <-    L.readFile corruptJPG  -- not existing fileAccess
--        putIOwords ["call_IO_Corrupt", showT . L.length $ f]  -- just to enforce strictness
--        return False   -- expect that read fials
--    assertEqual ( Left "/home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG: \
--        \hGetBufSome: hardware fault (Input/output error)") res

procFile = makeAbsFile "/proc/1/task/1/maps"
test_call_procp = do
    res <- runErr $ do
        f :: Text   <-    readFile2  procFile -- not allowed fileAccess
        return False   -- expect that read fials
    assertBool (isLeft res)
--    assertEqual ( Left "/proc/1/task/1/maps: openBinaryFile: permission denied (Permission denied)") res

test_createNewDirFile = do
    let fn = makeAbsFile "/home/frank/test/1.test"
    r <- runErr $ writeFileOrCreate2 fn ("testtext"::Text)
    assertEqual (Right () ) r

dir31 = "dir4test" :: FilePath
abs31 = "/home/frank/Workspace8/uniform/uniform-fileio" :: FilePath 
abs3131 = abs31 </> dir31

res3131 =  
    ["/home/frank/Workspace8/uniform/uniform-fileio/dir4test/testghci",
    "/home/frank/Workspace8/uniform/uniform-fileio/dir4test/testfile.txt",
    "/home/frank/Workspace8/uniform/uniform-fileio/dir4test/Setup.lhs",
    "/home/frank/Workspace8/uniform/uniform-fileio/dir4test/testgitignore"]

test_getDirCont1 = do
    res :: ErrOrVal [FilePath] <- runErr $ getDirContentFiles (abs3131) 
    assertEqual (Right res3131) res 

test_getDirCont2 = do 
    res :: ErrOrVal [Path Abs File]  <- 
                    runErr $ getDirContentFiles (makeAbsDir abs3131)
    assertEqual (Right (map makeAbsFile res3131)) res 
                -- (fmap makeAbsFile res3131) res 

res3132 ::   [FilePath]
res3132 = 
  ["dir4test/testghci", "dir4test/testfile.txt",
   "dir4test/Setup.lhs", "dir4test/testgitignore"]

test_getDirCont3 = do
    res :: ErrOrVal [FilePath] <- runErr $ getDirContentFiles (dir31) 
    assertEqual (Right res3132) res 

test_getDirCont4 = do 
    res :: ErrOrVal [Path Rel File]  <- 
                    runErr $ getDirContentFiles (makeRelDir dir31)
    assertEqual (Right (map makeRelFile res3132)) res 
                -- (fmap makeAbsFile res3131) res 

test_hidden1 = do 
    res :: ErrOrVal [FilePath]  <- 
                    runErr $ getDirContentNonHidden fp dir31
    assertEqual (Right (map makeRelFile res3132)) res 
                -- (fmap makeAbsFile res3131) res 

--test_md5_nonReadablep = do
--    res :: ErrOrVal (Maybe Text)  <- runErr $ getMD5 procFile
--    putIOwords ["test_md5_nonReadable res", showT res]
--    assertEqual (Left "getMD5 error for \"/proc/1/task/1/maps\"") res
--
--
--test_before = do
--    let fna = makeAbsFile "/home/frank/test/a.test"
--    let fnb = makeAbsFile "/home/frank/test/b.test"
--    r <- runErr $ isFileAbeforeB fna fnb
--    assertEqual (Right True ) r


--------------old test with filepath
--
--
--test_catch_error2 = do
--    res <- runErr $ do
--                            f :: Text <-  readFile2 ("xxxabcd" :: FilePath)
--                            return False
--                `catchError `
--                            \(e::Text) ->  return True
--    assertEqual (Right True) res
--
--test_call_IO = do
--    res <- runErr $ do
--        f :: String <-   callIO $ readFile "xxxabcd17"  -- not existing fileAccess
--        return False   -- expect that read fials
--    assertEqual ( Left "xxxabcd17: openFile: does not exist (No such file or directory)") res
--
--test_call_IO_L = do
--    res <- runErr $ do
--        f :: L.ByteString <-   callIO $ L.readFile "xxxabcd17"  -- not existing fileAccess
--        return False   -- expect that read fials
--    assertEqual ( Left "xxxabcd17: openBinaryFile: does not exist (No such file or directory)") res
--
----test_call_IO_Corrupt= do
----    res <- runErr $ callIO $ do
----        f :: L.ByteString <-    L.readFile corruptJPG  -- not existing fileAccess
----        putIOwords ["call_IO_Corrupt", showT . L.length $ f]  -- just to enforce strictness
----        return False   -- expect that read fials
----    assertEqual ( Left "/home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG: \
----        \hGetBufSome: hardware fault (Input/output error)") res
--
--test_call_proc = do
--    res <- runErr $ do
--        f   <-   callIO $ L.readFile "/proc/1/task/1/maps"  -- not existing fileAccess
--        return False   -- expect that read fials
--    assertEqual ( Left "/proc/1/task/1/maps: openBinaryFile: permission denied (Permission denied)") res
--
--test_md5_nonReadable = do
--    res :: ErrOrVal (Maybe Text)  <- runErr $ getMD5 ("/proc/1/task/1/maps" ::FilePath)
--    putIOwords ["test_md5_nonReadable res", showT res]
--    assertEqual (Left "getMD5 error for \"/proc/1/task/1/maps\"") res
--
--corruptJPG = "/home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG" ::FilePath
--
----test_fail = assertEqual "Fail intentionally just to insure that tests are run"(""::Text)
---- readable on santafe but not oporto
----test_md5_nonReadable2 :: IO ()
----test_md5_nonReadable2 = do
----        res :: ErrOrVal (Maybe Text)  <- runErr $ getMD5  corruptJPG
----        putIOwords ["test_md5_nonReadable corrupt jpg file", showT res]
----        -- does not catch the error?
----        assertEqual (Left "getMD5 error for \"/home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG\"") res
------   `catch` \(e::SomeException) -> do
------                putIOwords ["caught with catch in test_md5_nonReadable2 ", showT e]
------                return ()
--
---- not corrupt on santa fe, but on oporto
----test_md5_catch :: IO ()
----test_md5_catch = do
----        res3 :: ErrOrVal ByteString <- runErr $ callIO $ do
----                        res1 :: L.ByteString <-    L.readFile  corruptJPG
----                        let res2 = L.toStrict  res1
----                        return $!! res2
----        assertEqual (Left "/home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG: hGetBufSome: hardware fault (Input/output error)") res3
------   `catch` \(e::SomeException) -> do
------                putIOwords ["caught with catch in test_md5_catch ", showT e]
------                return ()
--
--test_symlink :: IO ()
--test_symlink = do
--    let t =   makeAbsFile "/bin/X11/X11"
--    isSymlink1 <- D.pathIsSymbolicLink   (OS.dropTrailingPathSeparator $ toFilePath t)
--    isSymlink2 <- D.pathIsSymbolicLink "/bin/X11/X11" -- (toFilePath t)
--    isSymlink3 <- D.pathIsSymbolicLink "/bin/X11/X11/" -- (toFilePath t)
--    assertEqual  (True, True, False) (isSymlink1, isSymlink2, isSymlink3)
--
--
