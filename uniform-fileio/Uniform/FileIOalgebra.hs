-----------------------------------------------------------------------------
--
-- Module      :  SVN.FileIO
-- Copyright   :  andrew u frank -
--
-- | the basic file io - translated for the Either or ErrorT signaling style
--  there are better (higher performance methods - replace while retaining conditions

-- reduced oct 2016 to small set
-- separate the operations from the OS and the operations which check for
-- undesirable characters in the filename and extension
-- approach: addExtension checks for bad extension characters
-- checks for filenames in the instances which use legalPathname
--
-- naming consistentl dir (not directory)
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
-- {-# LANGUAGE OverloadedStrings     #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.FileIOalgebra (
        module Uniform.FileIOalgebra
         , module Uniform.Error
         , module Uniform.Zero
         , module System.IO
            ) where


--import qualified Data.Text as T
--import qualified System.Posix  as Posix (FileStatus, Epoch)
import qualified System.Directory as D
import Uniform.FileStatus (FileStatus, EpochTime)
---- using uniform:
import           Uniform.Error --hiding ((<.>), (</>))
import           Uniform.Zero
-- import           Uniform.Filenames
import Uniform.Time (epochTime2UTCTime, UTCTime)
import System.IO (Handle, IOMode (..) )

class FileHandles t where
    write2handle :: Handle -> t -> ErrIO ()
    -- write a string or text to a handle
    readLine4handle :: Handle -> ErrIO t
    -- read a lline from a handle - used?

--class ListDir d f where
--    listDir' :: d -> ErrIO ([d],[f])
class FileSystemOps fp where
    getPermissions' :: fp -> ErrIO D.Permissions
    checkSymbolicLink :: fp -> ErrIO Bool
    -- ^ check if the fp points to a symbolic link
    -- better use isSimbolicLink (from FileStatus)

class DirOps fp where
    doesDirExist' :: fp -> ErrIO Bool
    createDir' :: fp -> ErrIO ()
    -- | write in a dir a new file with content
--    getDirPermissions  :: fp -> ErrIO D.Permissions

    createDirIfMissing' :: fp ->  ErrIO ()
    -- | creates the directory, if missing, recursive for path
    -- noop if dir exist
    renameDir' :: fp -> fp ->  ErrIO ()
    -- ^ rename directory old to new
    -- signals: getFileStatus: does not exist (No such file or directory)

    getDirectoryDirs' ::  fp -> ErrIO [fp]
    -- get the directories (but not . and ..)
    -- getDirectoryDirs' dir = filterM f =<< getDirCont  dir
    --     where f  =  doesDirExist'  
    getDirectoryDirsNonHidden' ::  fp -> ErrIO [fp]

    copyDirRecursive :: fp -> fp -> ErrIO ()
    -- | copy the directory content recursively, does not follow symlink
    -- implemented only for Path n Dir, not FilePath

    deleteDirRecursive :: fp -> ErrIO ()
    -- ^ delete a directory (even non empty), no error if not existing

class (Show fp) => FileOps fp   where
    doesFileExist' :: fp -> ErrIO Bool
--    doesFileOrDirExist :: fp -> ErrIO Bool
--    doesFileOrDirExist fp = do
--        d <- doesDirExist fp
--        f <- doesFileExist fp
--        return   (d || f)

--    getPermissions' :: fp -> ErrIO D.Permissions

    copyOneFile :: fp -> fp ->  ErrIO ()
    -- ^ copy a file from old to new
    renameOneFile :: fp -> fp ->  ErrIO ()
    -- ^ rename a file from old to new

    deleteFile :: fp -> ErrIO ()

--    assertDirNotExist :: fp -> ErrIO ()
--    -- ^ delete a directory (even non empty), if exist

    -- | get the directory content - if not existing Nothing, if empty Just []
    -- not returning the special entries   . and ..
        -- filenames completed with the filename calling
        -- check access and readable
        -- returns for filepath always an absolute path
        -- for Path Rel gives Path Rel results
    getDirCont :: fp ->  ErrIO [fp] --  (Maybe [String])
    -- | get the directory content - if not existing Nothing, if empty Just []
    -- not returning any hidden files
    -- alphabetic search to assure that equal directories have equal conten
    -- independent of file system internal structure
    -- filenames completed with calling fp
    -- only for filepath!
    getDirContNonHidden :: fp ->  ErrIO [fp]

    getMD5 :: fp -> ErrIO (Maybe Text)
    -- get MD5, but why Text  -- TODO

    getAppConfigDirectory :: ErrIO fp
    -- ^ find the .config directory path

    getSymbolicLinkStatus :: fp ->   ErrIO FileStatus
    -- ^ get status if exist (else Nothing)
    --   is the status of the link, does not follow the link

    getFileAccess :: fp -> (Bool, Bool, Bool) -> ErrIO Bool
    -- ^ check the read, write and execute permission on file
    -- dir get content needs execute,

--    isFileAbeforeB :: fp -> fp -> ErrIO Bool
--    -- ^ check if the first file A is touched before file B
--    -- uses modification time

    getFileModificationTime :: fp -> ErrIO EpochTime
    -- ^ get the modification time  (replaces isFileAbeforeB)
    getFileModificationUTCTime :: fp -> ErrIO UTCTime
    -- ^ get the modification time  in UTCTIme
    getFileModificationUTCTime = fmap epochTime2UTCTime . getFileModificationTime

-- operations on handle

    openFile2handle :: fp -> IOMode -> ErrIO Handle
-- use hPutStr :: Handle -> Text -> IO ()
        -- use closeFile2 without fp


-- read and write file in some monad
--class   FileOps3 fp fc m where
--
--    readFile3 :: fp -> m fc
--    writeFile3 :: fp -> fc -> m ()

-- | operations on dir to produce file
class (Show fd, Show ff) => FileOps2a fd ff where 
    getDirContentFiles :: fd -> ErrIO [ff]
    -- ^ getDirContentFiles dir = filterM f =<< getDirCont  dir
    --     where f x =  doesFileExist'   x

    getDirContentNonHiddenFiles :: fd -> ErrIO [ff]
--
--
---- | the operations on files with content
--
class (Show fp) =>
    FileOps2 fp fc where
--
    writeFile2 :: fp -> fc -> ErrIO ()
    -- write file if dir exist
    readFile2 :: fp -> ErrIO fc
    -- read file
    appendFile2 :: fp -> fc -> ErrIO ()
    -- append to a file




--    writeNewFileInDir :: fp -> fp -> fc ->  ErrIO ()
--    -- | create file in existing dir
--
--    writeFileCreateDir :: fp -> fp -> fc ->  ErrIO ()
--    -- | create file in existing dir
--
    writeFileOrCreate2 :: fp -> fc -> ErrIO ()
    -- ^ write or create a file
    -- if create dir if not exist (recursively for path)
    -- cannot be instantiated in the abstract
    -- because the getImmedateParentDir returns FilePath
--    writeFileOrCreate filepath st = do
--        let dir = getImmediateParentDir filepath
----        let (dir, fn, ext) = splitFilepath filepath
--        --        writeFileCreateDir dir fn st
--        --                  writeFileCreateDir dirpath filename st = do
----        let fp = dirpath `combine` filename
--        d <- doesDirExist' dir
--        --        f <- doesFileExist fp --
--        unless d $
--                createDirIfMissing' dir
--        writeFile2 filepath st

--

    readFileOrZero2 :: (FileOps fp, Zeros fc) => fp -> ErrIO fc
    -- | reads file, if not present, returns zero
    readFileOrZero2 fp = do
        f <- doesFileExist' fp
        if f
            then readFile2 fp
            else return zero
--
--

--    writeNewFileInDir dirpath filename st =  -- writeNewFileInDir (b2s dirpath) (b2s filename) (b2s st)
--        do
--            let fp = dirpath `combine` filename
--            d <- doesDirectoryExist dirpath
--            f <- doesFileExist fp -- filename
--            if not d  then throwError . unwords $
----            signalf  DirNotExist
--                    ["writeNewFileInDir"
--                        , show dirpath, "not existing"]
--                        else if f then throwError . unwords $
----                            signalf  TargetExist
--                            ["writeNewFileInDir"
--                        , show filename, "exist"]
--                else  writeFile2 fp st
--
--    writeFileCreateDir dirpath filename st = do
--        let fp = dirpath `combine` filename
--        d <- doesDirectoryExist dirpath
--        f <- doesFileExist fp --
--        unless d $
--                createDirIfMissing dirpath
--        writeFile2 fp st
--
----
--
--
---- | the signals for fileops
--data FileOpsSignals = TargetExist   -- ^ the file or dir exists
--        | DirNotExist
--        | DirEmpty
--        | SourceExist
--        | SourceNotExist
--        | SystemIOerror
--        | SomeReadError
--        deriving (Show, Eq)



