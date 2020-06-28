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
{-# LANGUAGE PackageImports #-}


module Main     where      -- must have Main (main) or Main where


--import System.Exit

import         "HTF"  Test.Framework
import     {-@ HTF_TESTS @-}       Uniform.Json_test
-- import     {-@ HTF_TESTS @-}       Uniform.Yaml_test

import           Uniform.Strings

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
