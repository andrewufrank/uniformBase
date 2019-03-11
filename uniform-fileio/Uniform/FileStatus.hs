-----------------------------------------------------------------------------
--
-- Module      :  uniform-FileIO
-- Copyright   :  andrew u frank -
--
-- | the routines to take apart the file status
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE
    MultiParamTypeClasses
    , TypeSynonymInstances
    , FlexibleInstances
    , FlexibleContexts
--    , DeriveFunctor
    , ScopedTypeVariables
--    , UndecidableInstances
    , TypeFamilies
    , OverloadedStrings

    #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# OPTIONS -w #-}

module Uniform.FileStatus (
         getFileStatus, isDirectory, isRegularFile
        , getFileStatus'
        , isSymbolicLink
--        , getSymbolicLinkStatusFP
--        , createSymbolicLink, renameLink
--        , doesExist
        , getModificationTimeFromStatus  -- TODO is this correct export?
        , getFileSize
        , P.EpochTime, P.FileStatus
--        , getSymbolicLinkStatusX
                   ) where

--import qualified Data.Text as T
--import Path
--import Path.IO
import qualified System.Posix as P
import qualified System.Directory as S
----import Basics
import Uniform.Error
import Uniform.Zero
import Uniform.Strings
import Uniform.Filenames

--import Test.Framework

-- new approach

--getSymbolicLinkStatusFP :: FilePath  -> ErrIO ( P.FileStatus)
---- ^ get status if exist (else Nothing)
----   is the status of the link, does not follow the link
----
--getSymbolicLinkStatusFP  fp = do
------    putIOwords ["fileio getSymbolicLinkStatus", fp]
--    st <- callIO $ P.getSymbolicLinkStatus fp
------    putIOwords ["fileio getSymbolicLinkStatus done", fp]
--    return   st
----  `catchError` (\s -> do
----            putIOwords ["fileio getSymbolicLinkStatus not found", showT fp]
----            return Nothing)
----  where fp = unL lfp

unL = toShortFilePath

--getFileStatus :: Path df ra -> ErrIO P.FileStatus
getFileStatus fp = callIO $ P.getFileStatus . unL $ fp

getFileStatus' :: FilePath  -> ErrIO P.FileStatus
getFileStatus' fp = callIO $ P.getFileStatus   fp


isRegularFile :: P.FileStatus -> Bool
isRegularFile = P.isRegularFile
isDirectory :: P.FileStatus -> Bool
isDirectory = P.isDirectory
isSymbolicLink :: P.FileStatus -> Bool
isSymbolicLink = P.isSymbolicLink
getModificationTimeFromStatus :: P.FileStatus -> P.EpochTime
getModificationTimeFromStatus = P.modificationTime
getFileSize = P.fileSize

createSymbolicLink :: Path df ra -> Path df ra-> ErrIO ()
createSymbolicLink fn tn = do
    putIOwords ["createSymbolidLink", showT fn , "to", showT tn]
    callIO $ P.createSymbolicLink (unL fn) (unL tn)


renameLink :: Path df ra  -> Path df ra  -> ErrIO ()
renameLink old new = callIO $ P.rename (unL old) (unL new)
-- should check that this is a link and existing etc.

--doesExist :: Path df ra  -> ErrIO Bool
---- ^ test if dir, file or link exist
--doesExist lfp = callIO $ do
--
--    f <- doesFileExist lfp
--    d <- doesDirectoryExist lfp
--    s <- pathIsSymbolicLink lfp
--    return (f || d || s)
--  where fp = unL lfp


----from fay
---- | Join for Maybe.
--joinMaybe :: Maybe (Maybe a) -> Maybe a
--joinMaybe (Just (Just x)) = Just x
--joinMaybe _ = Nothing



--



