-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Example_test
--
-- | a miniaml set of  
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns          #-}
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

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
{-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
{-# OPTIONS_GHC  -fno-warn-type-defaults #-}


module Uniform.Ftp_test where

import           Test.Framework
import           Uniform.Strings hiding ((</>), (<.>), (<|>))
import Uniform.Ftp
import Uniform.FileIO

import Control.Exception

test_0 = assertEqual "" "x"

mainStateIOa ::  FTPstate  ()
-- tests for changing dirs 
mainStateIOa = do
        lift $ putIOwords ["main2 - in IO ()"]
        h <- ftpConnect  
        d1 <- ftpDir  -- main dir 
        lift $ putIOwords ["\ndir root", unlines'  d1]
        ftpChangeDir (makeRelDir "test.gerastree.at")
        d2 <- ftpDir 
        lift $ putIOwords ["\ndir test", unlines'  d2]

        ftpChangeDir (makeRelDir "af/af-publicationlists/topics")
        -- relative - works added
        d3 <- ftpDir 
        lift $ putIOwords ["\ndir topics", unlines'  d3]

        ftpChangeDir (makeAbsDir "/test.gerastree.at/af/af-publicationlists/")
        -- relative - works added
        d4 <- ftpDir 
        lift $ putIOwords ["\ndir publicationlists", unlines'  d4]

        -- ftpChangeDir 
        -- dirX <- callIO $ push2 h
        return ()

mainStateIOb ::  FTPstate  ()
-- tests for uploading files  
mainStateIOb = do
        lift $ putIOwords ["testing uploads - in IO ()"]
        h <- ftpConnect  
        d1 <- ftpDir  -- main dir 
        lift $ putIOwords ["\ndir root", unlines'  d1]
        ftpChangeDir (makeRelDir "test.gerastree.at")
        d2 <- ftpDir 
        lift $ putIOwords ["\ndir test", unlines'  d2]

      -- writing to test 
        ftpUpload2currentDir (wdir </> makeRelFile "testfile.txt") 
              (makeRelFile "test1.txt")
        d3 <- ftpDir 
        lift $ putIOwords ["\ndir test with test1.txt", unlines'  d3]
        -- written relative path 
        return ()

        -- writing to test 
        ftpUpload (wdir </> makeRelFile "testfile.txt") 
            (makeAbsFile "/test.gerastree.at/af/test2.txt")
        d4 <- ftpDir 
        lift $ putIOwords ["\ndir test with test1.txt", unlines'  d4]
        -- written relative path 
        ftpChangeDir (makeAbsDir "/test.gerastree.at/af/" )
        d5 <- ftpDir 
        lift $ putIOwords ["\ndir test with test2.txt", unlines'  d5]

        return ()

        mainStateIOb ::  FTPstate  ()
-- tests for uploading dir  
mainStateIOc = do
        lift $ putIOwords ["testing uploads - in IO ()"]
        h <- ftpConnect  
        d1 <- ftpDir  -- main dir 
        lift $ putIOwords ["\ndir root", unlines'  d1]
        ftpChangeDir (makeRelDir "test.gerastree.at")
        d2 <- ftpDir 
        lift $ putIOwords ["\ndir test", unlines'  d2]

      -- writing to test 
        let targetDir = makeAbsDir "/test.gerastree.at/dir4test"
        ftpMakeDir targetDir 
        ftpUploadFilesFromDir (
                    (wdir </> makeRelDir "dir4test") :: Path Abs Dir)
                    targetDir
        ftpChangeDir targetDir
        d3 <- ftpDir 
        lift $ putIOwords ["\ndir test with test1.txt", unlines'  d3]
        -- written relative path 
        return ()

        -- -- writing to test 
        -- ftpUpload (wdir </> makeRelFile "testfile.txt") 
        --     (makeAbsFile "/test.gerastree.at/af/test2.txt")
        -- d4 <- ftpDir 
        -- lift $ putIOwords ["\ndir test with test1.txt", unlines'  d4]
        -- -- written relative path 
        -- ftpChangeDir (makeAbsDir "/test.gerastree.at/af/" )
        -- d5 <- ftpDir 
        -- lift $ putIOwords ["\ndir test with test2.txt", unlines'  d5]

        return ()
wdir = makeAbsDir "/home/frank/Workspace8/uniform/uniform-ftp/"
