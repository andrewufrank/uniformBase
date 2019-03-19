-----------------------------------------------------------------------------
--
-- Module      :  Uniform.TestHarness
--
-- | two functions to deal wtih tests which
-- store data on disk
 -- interface must be in the wrapped Path, to allow the reading ??
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns          #-}
--{-# LANGUAGE DeriveDataTypeable    #-}
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

{-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is depreceiated but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.Test.Utils (module Uniform.Test.Utils
    , module Uniform.FileIO
    , ppShow


        )  where

import           Safe
import           Test.Framework
import Uniform.FileIO
import Text.Show.Pretty (ppShow )
import qualified Path.IO as Path.IO (doesFileExist, getAppUserDataDir)
        -- necessary for operations in IO
import Text.Read
import Data.Time (UTCTime (..))

-- operations are in IO not ErrIO, therefore here and not in fileio
getLitTextTestDir ::  IO (Path Abs Dir)
getLitTextTestDir  = Path.IO.getAppUserDataDir "LitTextTest"
-- operations are in IO not ErrIO, therefore here and not in fileio
getLitTextTestDir2 :: Text -> IO (Path Abs Dir)
getLitTextTestDir2 progName = Path.IO.getAppUserDataDir . t2s $ progName

getLitTextTestDir3 :: Text -> ErrIO (Path Abs Dir)
getLitTextTestDir3 progName =  callIO . Path.IO.getAppUserDataDir . t2s $ progName

doesFileExistWrapped :: Path Abs File -> IO Bool
doesFileExistWrapped fn = Path.IO.doesFileExist (unPath fn)

readStartFile :: Bool -> Path Abs Dir -> FilePath -> IO String
-- ^ read the start file as string
readStartFile testvardebug  testDataDir startfile = do
    let fn0 =  testDataDir   </> startfile :: Path Abs File
    when testvardebug $ putIOwords ["test2a readStartFile filename input ", showT fn0]
    f0 :: String <- readFile (toFilePath fn0)
    return f0

readStartFile3 :: Bool -> Path Abs Dir -> FilePath -> ErrIO String
-- ^ read the start file as string
readStartFile3 testvardebug  testDataDir startfile = do
    let fn0 =  testDataDir   </> startfile :: Path Abs File
    when testvardebug $ putIOwords ["test2a readStartFile3 filename input ", showT fn0]
    f0 :: String <- readFile2 fn0
    return f0


checkResult :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
            => Bool ->  Path Abs Dir -> FilePath -> b -> ErrIO Bool
checkResult testvardebug testDataDir resfile tt1 = do
        let fn = testDataDir </> resfile :: Path Abs File
        let fnx = testDataDir </> (resfile ++ "-failed" ) :: Path Abs File
        when testvardebug $
            putIOwords ["\n\ncheckResult test ", s2t resfile, showT fn]

        fnotexist <- fmap not $ doesFileExist' fn
        when testvardebug $
                putIOwords ["checkResult file not exist:  doesFileExist' = ", showT fnotexist]

        -- write result file, if not exist
        when  fnotexist $ do
            writeFileOrCreate2 ( fn ) .s2t  $ showTestH tt1
            putIOwords ["checkResult new result file written", showT fn
                                , "length ", showT . length . showTestH $ tt1]

        -- write failed file
        writeFileOrCreate2 ( fnx ) .s2t  $ showTestH tt1
        when testvardebug $
                putIOwords ["checkResult new faile file written", showT fn
                            , "length ", showT . length . showTestH $ tt1]
        -- a result file and a failed file exist
        checkResultAndDeleteFailedIfEqual testvardebug fn fnx

    `catchError` (\e -> do
        putIOwords ["catchError ", showT e]

        return  False
             )

checkResultAndDeleteFailedIfEqual ::
--    (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)  =>
             Bool -> Path Abs File -> Path Abs File -> ErrIO Bool

checkResultAndDeleteFailedIfEqual testvardebug fn fnx  = do
    when testvardebug $
            putIOwords ["checkResultAndDeleteFailedIfEqual for fn and fnx", showT fn, showT fnx]
    rfn :: Text <- readFile2  fn
    rfnx :: Text <- readFile2 fnx   -- the files are written as text
    let testres = rfn == rfnx
    when testvardebug $
            putIOwords ["checkResultAndDeleteFailedIfEqual testres", showT testres]
    when testres $ do
        deleteFile fnx
        when testvardebug $
                putIOwords ["checkResultAndDeleteFailedIfEqual deleted fnx",   showT fnx]
    when testvardebug $
            putIOwords ["checkResultAndDeleteFailedIfEqual end"]
    return testres




class ShowTestHarness t where
    showTestH :: Show t =>  t -> String
    -- used for the writing to the files
    showTestH = ppShow
--    readTestH :: Read t => String -> t
    -- all reads from file are with readTestH2
--    readTestH = readNote "showTestHarness t"
    readTestH2 :: Read t => String -> String ->  t
    readTestH2 msg = readNote msg
    readTestH2e :: Read t => String -> String -> Either String t
    readTestH2e msg = readEither
--                readNote msg
--                either (throwErrorT) id $ readEither ("readTestH2 no parse " <> msg)

--instance (Show t , ShowTestHarness t) => ShowTestHarness [t] where
--
--    showTestH t =   ppShow t run

instance ShowTestHarness Bool

instance ShowTestHarness UTCTime where
    showTestH = show
    readTestH2 msg = readNote msg

instance  ShowTestHarness Text where
    -- to avoid the additional "" added when show text
    -- but the read must compensate!
    -- this is necessary that json files (and other with "") can be read
    showTestH = t2s
--    readTestH = readNote "showTestHarness Text" . show
    readTestH2 msg = readNote (  msg) . show
    readTestH2e msg = readEither . show
--
instance  ShowTestHarness String where
    -- to avoid the additional "" added when show text
    showTestH = id
--    readTestH = readNote "showTestHarness String " -- . show
    readTestH2 msg = readNote (  msg) . show
    readTestH2e msg = readEither . show

instance  ShowTestHarness LazyByteString where
    -- to avoid the additional "" added when show text
    -- but the read must compensate!
    -- this is necessary that json files (and other with "") can be read
    showTestH = bb2s . bl2b
--    readTestH = readNote "showTestHarness Text" . show
    readTestH2 msg = readNote (  msg) . show
    readTestH2e msg = readEither . show
--
--instance  ShowTestHarness () where
--    showTestH = show
--    readTestH = readNote "showTestHarness bottom () " -- . show
----    readTestH2 msg = readNote (  msg) . show
--
--instance ShowTestHarness Int where
--    showTestH = show
--    readTestH = readNote "showTestHarness Int " -- . show
----    readTestH2 msg = readNote (  msg) . show

--instance  ShowTestHarness t where
----    showTestH tx@(a:as)= show tx
----    showTestH tx = show tx


