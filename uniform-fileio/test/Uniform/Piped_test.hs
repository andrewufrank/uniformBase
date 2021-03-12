-----------------------------------------------------------------------------
--
-- Module      :  piped
-- Copyright   :  andrew u frank -
--
-- | the recursive access to many files not blocking
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE
    MultiParamTypeClasses
--    , TypeSynonymInstances
    , FlexibleInstances
    , FlexibleContexts
    , ScopedTypeVariables
    , UndecidableInstances
    , OverloadedStrings
--    , TypeFamilies
    #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module  Uniform.Piped_test where

import qualified Pipes as Pipe
import  Pipes ((>->))
import qualified Pipes.Prelude as PipePrelude
----import Control.Monad (forM_)
--
----import System.Directory (doesDirectoryExist, getDirectoryContents)
--import System.Environment (getArgs)
----import System.FilePath ((</>))
----import System.IO (openFile, IOMode (..), hClose)
--
------ using uniform:
--import Uniform.Error
----import Uniform.Zero
import Uniform.Strings hiding ((<.>), (</>))
----
import Uniform.Filenames
----import Uniform.FileIO
import Uniform.FileStrings (openFile2handle, closeFile2, IOMode(..))
--import Uniform.Filenames
--import Data.List (sort)

import Test.Framework
--import Test.Invariant
import Uniform.Piped
import qualified Path.IO as Path.IO (makeAbsolute)



test_recursive = do
    let testdir = makeRelDir "testDirFileIO"
    let resfileN = makeRelFile "testDirResN"
    let resfile0 = makeRelFile "testDirRes0"
    testdir2 <- fmap Path $ Path.IO.makeAbsolute (unPath testdir)
    runErr $ do
        hand <-   openFile2handle resfileN WriteMode
        Pipe.runEffect $
            getRecursiveContents testdir2
            >-> PipePrelude.map  toFilePath
    ----    >-> P.stdoutLn
            >-> PipePrelude.toHandle hand
        closeFile2 hand
    res0  ::Text <-  readFile5  resfile0
    resN :: Text <-  readFile5 resfileN
    assertEqual res0 resN



testDir =  makeAbsDir "/home/frank/Workspace8/uniform-fileio/testDirFileIO"
test_getRec = do
    res <- runErr $ pipedDo testDir (showT)
    assertEqual (Right ()) res
    -- check manually

----for testing:
readFile5 :: Path ar File -> IO Text
readFile5 = fmap s2t .readFile . toFilePath

pipedDo :: Path Abs Dir -> (Path Abs File -> Text) -> ErrIO ()
pipedDo path transf =  do
  Pipe.runEffect $
    getRecursiveContents path
    >-> PipePrelude.map (t2s . transf)
    >-> PipePrelude.stdoutLn

--pipedDoIO :: Path Abs File -> Path Abs Dir -> (Path Abs File -> ErrIO Text) -> ErrIO ()
---- | write to the first filename the operation applied to the dir tree in the second
---- first path must not be non-readable dir or
--pipedDoIO file path transf =  do
--    hand <-   openFile2handle file WriteMode
--    Pipe.runEffect $
--                getRecursiveContents path
--                >-> PipePrelude.map (fmap t2s . transf)  -- some IO type left?
--                --    >-> P.stdoutLn
--                >-> PipePrelude.toHandle hand
----                >-> (\s -> PipePrelude.toHandle hand (s::String))
--    closeFile2 hand

