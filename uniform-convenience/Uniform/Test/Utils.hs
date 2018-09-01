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


        )  where

import           Safe
import           Test.Framework
import Uniform.FileIO
import Text.Show.Pretty
import qualified Path.IO as Path.IO (doesFileExist, getAppUserDataDir)
        -- necessary for operations in IO
import Text.Read

-- operations are in IO not ErrIO, therefore here and not in fileio
getLitTextTestDir ::  IO (Path Abs Dir)
getLitTextTestDir  = fmap Path $ Path.IO.getAppUserDataDir "LitTextTest"
-- operations are in IO not ErrIO, therefore here and not in fileio
getLitTextTestDir2 :: Text -> IO (Path Abs Dir)
getLitTextTestDir2 progName = fmap Path $ Path.IO.getAppUserDataDir . t2s $ progName

getLitTextTestDir3 :: Text -> ErrIO (Path Abs Dir)
getLitTextTestDir3 progName = fmap Path . callIO . Path.IO.getAppUserDataDir . t2s $ progName

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

--checkResultIOop :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
--            => Bool -> Path Abs Dir -> FilePath ->  b -> ErrIO Bool
---- ^ check the result when it may be an error
--checkResultIOop testvardebug  testDataDir resfile tt1  = do
--    when True $ -- testvardebug $
--        putIOwords ["test3 testVar3FileIO B", "result",  showT t1]
--    case t1 of
--        Left msg -> do
----                    when testvardebug $
--                    putIOwords ["test3 Left testVar3FileIO\n"
--                     , "resultFile:", s2t resfile
----                     , "possibly only the resultfile not existing - create by hand"
--                       , "\nMessage:", msg, "."]
--                    return False
--        Right tt1 -> do
--                putIOwords ["test3 testVar3FileIO C check result"]
--                r <-  checkResult testvardebug testDataDir resfile tt1
--                putIOwords ["test3 testVar3FileIO C check result gives", showT r ]
--                return  r

checkResult :: (Zeros b, Eq b, Show b, Read b, ShowTestHarness b)
            => Bool ->  Path Abs Dir -> FilePath -> b -> ErrIO Bool
checkResult testvardebug testDataDir resfile tt1 = do
        let fn = testDataDir </> resfile :: Path Abs File
        let fnx = testDataDir </> ("x" ++ resfile  ) :: Path Abs File
        when testvardebug $
            putIOwords ["checkResult test", s2t resfile, showT fn, "\n"]
        fnexist <- fmap not $ doesFileExist' fn
        putIOwords ["checkResult doesFileExist'", showT fnexist, "\n"]
        when fnexist $ throwErrorT ["e1 resultFile does not exist"]
        r0 :: Text  <- readFile2  (toFilePath fn)
        when (null' r0) $ throwErrorT ["e2 resultFile is empty"]
        let
            r1m =  readTestH2e "e3 no parse " (t2s r0)  `asTypeOf` (Right tt1)
        r1 <- either (\m -> throwErrorT ["e3 result file does not parse", s2t m]) return r1m
        when True $ -- testvardebug $
            putIOwords ["test3 checkResult resultFile:", s2t resfile
            , "\ninputFile content read\n", showT r1]
        let testres = r1 == tt1
        --                unless (testres && testvardebug) $ do
        when testvardebug  $ do
            putIOwords ["checkResult test3a  "
                    , showT testres, "\n", showT tt1]
            putIOwords ["checkResult test3a  expected file"
                            , show' fn, "contains\n", showT r1]
        unless testres $ do
            when testvardebug  $ do
                putIOwords ["checkResult test4 - result different from expected"
                    , " - write NEW result"
                        , showT testres, "\n", showT tt1]
            writeFile2  fnx  . s2t  $ showTestH tt1
        return testres
    `catchError` (\e -> do
        let fn = testDataDir </> resfile :: Path Abs File
        let fnx = testDataDir </> ("x" ++ resfile  ) :: Path Abs File
        f <- case (headNote "no error msg" . words' $ e) of
            "e1" -> do  -- file not present
                putIOwords ["catchError test4  - e1 no previous file existing"]
                writeFileOrCreate2 ( fn ) .s2t  $ showTestH tt1
                putIOwords ["catchError test4  - file written", showT fn
                            , "length ", showT . length . showTestH $ tt1]
                return True

            "e2" -> do  -- file empty
                putIOwords ["catchError test4  - e2 result file empty", showT fn]
                deleteFile fn
                putIOwords ["deleted, but not written", showT fn
                                , "length ", showT . length . showTestH $ tt1]
                writeFile2  fn  . s2t  $ showTestH tt1
                putIOwords ["catchError test4  - file written", showT fn
                            , "length ", showT . length . showTestH $ tt1]
                return True

            "e3" -> do  -- file not parsing
                putIOwords ["catchError test4  - e3 no parse for result file, wrote xfile"
                        , showT fnx]
                writeFile2  fnx   .s2t   $ showTestH tt1
                putIOwords ["catchError test4  - file written", showT fn
                            , "length ", showT . length . showTestH $ tt1]
                return False
        return  f
             )


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


