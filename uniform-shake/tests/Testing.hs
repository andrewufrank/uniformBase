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
import  {-@ HTF_TESTS @-}         Uniform.Shake_test

import           Uniform.Strings

--import TestingFileIO

-- test_fileio = assertBool False

main = do
    putIOwords ["HTF errorTest.hs:\n uniform-error test"]
    r <- htfMainWithArgs ["--quiet"] htf_importedTests
    putIOwords ["HTF end errorTest.hs:\n posTest", showT r]
    -- r2 <- errorTest
    -- putIOwords [" error test end\n  ", showT r2]
    return r
