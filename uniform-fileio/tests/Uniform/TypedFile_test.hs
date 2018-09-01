{-# OPTIONS_GHC -F -pgmF htfpp #-}
--{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- {-# OPTIONS -Wall #-}
-- {-# OPTIONS -fno-warn-missing-signatures #-}
{-# OPTIONS -w #-}

module Uniform.TypedFile_test   where

import Test.Framework

--import           Uniform.Error
import           Uniform.FileIOalgebra (Handle)
import           Uniform.Filenames
import           Uniform.FileStrings
--import           Uniform.FileIO (EpochTime, getFileModificationTime)
import           Uniform.FileStatus
--import           Uniform.Strings hiding ((</>))

import Uniform.TypedFile


textLinesFile = makeTyped (Extension "txt")  ::TypedFile5 [Text] ()
dir1 = makeAbsDir "/home/frank/"
file1 = makeRelFile "aaa"
ct = ["eins", "zwei"]
test_write = do
    r <- runErr $ write5 dir1 file1 textLinesFile ct
    assertEqual (Right () ) r

test_read = do
    r <- runErr $ read5 dir1 file1 textLinesFile
    assertEqual (Right ct ) r

