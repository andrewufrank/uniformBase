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
import           Uniform.Strings  
import qualified Data.ByteString.Lazy   as L

import Uniform.TypedFile


textLinesFile = makeTyped (Extension "txt")  ::TypedFile5 [Text] ()
dir1 = makeAbsDir "/home/frank/"
file1 = makeRelFile "aaa"
ct = ["eins", "zwei"] :: [Text]
test_write = do
    r <- runErr $ write5 dir1 file1 textLinesFile ct
    assertEqual (Right () ) r

test_read = do
    r <- runErr $ read5 dir1 file1 textLinesFile
    assertEqual (Right ct ) r

-- data CompressedByteString
-- a gzip compressed bytestring -- 
gzippedTriples = TypedFile5 {tpext5 = Extension "triples.gzip"} 
                :: TypedFile5 L.ByteString [Text]

test_gz4txt = do 
    r <- runErr $ write8 (dir1 </> file2) gzippedTriples  ct
    assertEqual (Right ()) r 

file2 = makeRelFile "b2"

test_gz4back = do 
    r <- runErr $ read8 (dir1 </> file2) gzippedTriples  
    assertEqual (Right ct) r 

instance TypedFiles7 L.ByteString  [Text]    where
    unwrap7 =  compress . b2bl . t2b . showT
    wrap7 = read . t2s . bb2t . bl2b . decompress  
    -- - | the a is the base type
    -- -- which is written on file, b is the type for input and output
    -- class FileHandles a => TypedFiles7 a b where
    --     wrap7 :: a -> b
    --     unwrap7 :: b -> a
    

