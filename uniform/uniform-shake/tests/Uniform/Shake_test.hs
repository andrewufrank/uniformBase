-----------------------------------------------------------------------------
--
-- Module      :  FileIO.ByteString
--
-- | the implementation for filepath encoded as bytestring (RawFilePath = FilePathX)
--
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- {-# LANGUAGE DeriveFunctor           #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances       #-}
-- {-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}


module Uniform.Shake_test where


import           Test.Framework
import Uniform.Shake 
import Uniform.FileIO 
-- import Test.Invariant
-- import Uniform.ByteString
--import qualified Data.ByteString as BS
--
-- tests from filenames:
f1 = "afile" :: FilePath  -- not legal?
f2 = "afile.ext" :: FilePath
f3 = "/somedir/more/afile.ext"  :: FilePath
f4 = "afile.gut.ext" :: FilePath 
-- test_emptyExt = assertEqual "" (getExtension f1)
-- test_emptyExt0 = assertEqual "" (getExtension f0)
-- test_getExt = assertEqual "ext" (getExtension f2)
-- test_hasExt = assertBool $  hasExtension "ext" f2
-- test_hasExt2 = assertBool $  hasExtension "ext" f3
-- test_addExt = assertEqual ( f2) $  addExtension "ext" f1
test_removeExt = assertEqual f1 (removeExtension f2)
test_setExt = assertEqual ("afile.txt") (setExtension "txt" f2)

test_removeExt2 = assertEqual f1 (removeExtension . removeExtension $ f4)

f4p = makeRelFile f4 
f1p = makeRelFile f1  
f2p = makeRelFile f2  

test_removeExt2path  = assertEqual f1p 
               (removeExtension . removeExtension $ f4p)

-- test 
f5 = "afile.new" :: FilePath 
f5p = makeRelFile f5
test_replaceExtension = assertEqual f5p (replaceExtension' "new"  f2p)
test_replaceExtension2 = assertEqual f5p (replaceExtension2 "new"  f4p)
test_replaceExtension2x = assertEqual f5p (f4p $--<.> "new")

