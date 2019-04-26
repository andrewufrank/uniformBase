-----------------------------------------------------------------------------
--
-- Module      :   top tests for layout
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

import            Test.Framework
import     {-@ HTF_TESTS @-}       Uniform.FileStrings_test
-- import {-@ HTF_TESTS @-} Uniform.ByteString_test
import     {-@ HTF_TESTS @-}       Uniform.Filenames_test
import     {-@ HTF_TESTS @-}       Uniform.PathShowCase_test
import    {-@ HTF_TESTS @-}        Uniform.FileStatus_test
-- import     {-@ HTF_TESTS @-}       Uniform.Piped_test
import    {-@ HTF_TESTS @-}        Uniform.TypedFile_test

import           Uniform.Strings
import Uniform.Error 
--import TestingFileIO

test_fileio = assertBool False

main :: IO ()
main = do

    putIOwords ["HTF LayoutTest.hs:\n posTest"]
--    htfMainWithArgs ["--quiet"] htf_importedTests
    htfMain   htf_importedTests
    putIOwords ["HTF end LayoutTest.hs:\n posTest"]
    runTest test_fileio
    return ()

file1test = do
    putStrLn "file1test"
    -- push2
    -- runErrorVoid $ do 
    test_hidden1
    return () 