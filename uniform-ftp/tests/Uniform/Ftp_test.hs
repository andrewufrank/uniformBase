-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Example_test
--
-- | a miniaml set of  
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- {-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
{-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
{-# OPTIONS_GHC  -fno-warn-type-defaults #-}


module Uniform.Ftp_test where

import           Test.Framework
import           Uniform.Strings hiding ((</>), (<.>), (<|>))
import Uniform.Ftp
import Uniform.FileIO
-- import Control.Monad.Trans.State 
import Uniform.Time 

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
        -- return ()

        -- writing to test 
        ftpUpload (wdir </> makeRelFile "testfile.txt") 
            (makeAbsFile "/test.gerastree.at/af/test2.txt")
        d4 <- ftpDir 
        lift $ putIOwords ["\ndir test with test1.txt", unlines'  d4]
        -- written relative path 
        ftpChangeDir (makeAbsDir "/test.gerastree.at/af/" )
        d5 <- ftpDir 
        lift $ putIOwords ["\ndir test with test2.txt", unlines'  d5]

        -- return ()

mainStateIOc ::  FTPstate  ()
-- tests for uploading dir  
mainStateIOc = do
        lift $ putIOwords ["testing uploads mainStateIOc"]
        h <- ftpConnect  
        d1 <- ftpDir  -- main dir 
        lift $ putIOwords ["\ndir root", unlines'  d1]
        ftpChangeDir (makeRelDir "test.gerastree.at")
        d2 <- ftpDir 
        lift $ putIOwords ["\ndir test", unlines'  d2]

      -- writing to test 
        lift $ putIOwords ["\ntesting uploads mainStateIOc makedir"]
        let targetDir = makeAbsDir "/test.gerastree.at/dir4test"
        ftpMakeDir targetDir 
        lift $ putIOwords ["\ntesting uploads mainStateIOc upload "]
        ftpUploadFilesFromDir test0 (
                    (wdir </> makeRelDir "dir4test") :: Path Abs Dir)
                    targetDir
        ftpChangeDir targetDir
        d3 <- ftpDir 
        lift $ putIOwords ["\ndir test with test1.txt", unlines'  d3]
        -- written relative path 
        return ()

mainStateIOd ::  FTPstate  ()
mainStateIOd = do
        lift $ putIOwords ["testing uploads mainStateIOd"]
        h <- ftpConnect  

        lift $ putIOwords ["\ntesting uploads mainStateIOd makedir"]
        let targetDir = makeAbsDir "/test.gerastree.at/dir10test"
        let sourceDir = (wdir </> makeRelDir "dir4test") :: Path Abs Dir
        -- ftpMakeDir targetDir 
        lift $ putIOwords ["\ntesting uploads mainStateIOd upload "]
        ftpUploadDirsRecurse test0 sourceDir targetDir 
        ftpChangeDir targetDir
        d3 <- ftpDir 
        lift $ putIOwords ["\ndir test with test1.txt", unlines'  d3]

        return ()

wdir = makeAbsDir "/home/frank/Workspace8/uniform/uniform-ftp/"

main3 = runErrorVoid $ do
        currentTime <- getCurrentTimeUTC 
        (a,s)  <- runStateT  
                     (ftpUploadDirsRecurse test0 bakedPath (makeAbsDir "/test.gerastree.at/"))
                     ftp0
        putIOwords ["uploadTest completed", showT currentTime]             
        return () 

bakedPath = makeAbsDir "/home/frank/Workspace8/ssg/docs/site/baked"
-- uploadBaked = ftpUploadRecurse 

lastUploadFile = makeRelFile "lastload.txt" :: Path Rel File 

main4 = runErrorVoid $ do
        -- test with file stored 
        lastUpload1 <- readFile2 lastUploadFile   -- current dir - same as settingsfile 
        let lastUpload = read lastUpload1 :: UTCTime 
        let testWithLastTime  = testNewerModTime lastUpload

        putIOwords ["uploadTest started - last was ", showT lastUpload]             

        (a,s)  <- runStateT  
                     (ftpUploadDirsRecurse testWithLastTime bakedPath 
                        (makeAbsDir "/test.gerastree.at/")
                        )
                     ftp0

        currentTime <- getCurrentTimeUTC 
        writeFile2 lastUploadFile (show currentTime)

        putIOwords ["uploadTest completed", showT currentTime]             
        return () 


test1, test0 :: Path Abs File -> ErrIO Bool 
test1 = testNewerModTime (read "2019-04-11 12:00:00 UTC" :: UTCTime)  
test0 = (\f -> return    True )

test_time1 = do
        res <- runErr $ do  
                ct <- getCurrentTimeUTC 
                b <- testNewerModTime ct (wdir </> makeRelFile "testfile.txt")
                return b
        assertEqual (Right False) res

test_time2 = do
        res <- runErr $ do  
                
                b <- test1 (wdir </> makeRelFile "testfile.txt")
                return b
        assertEqual (Right False) res