-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Example_test
--
-- | import examples to test with  {-@ HTF_TESTS @-}
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


module Uniform.PathShowCase_test where

import           Test.Framework
import           Uniform.Strings hiding ((</>), (<.>), (<|>))
import  Uniform.PathWrapper
import qualified Path as P
import Control.Exception

data T1 = T1 {a1 :: Text
            , p1 :: Path Rel File
            , n1 :: Int
                } deriving (Show, Read, Eq)

t1 = T1 "some text" s2  37 
st1 = showT t1 
rt1 = read. t2s $ st1 :: T1 

t2 = T1 "anoher long, atest text" r6  2009
rt2 = read . show $ t2 :: T1 


-- s1 = Path Rel File "testFile.txt" :: Path Rel File 
-- r1 = "Path Rel File \"testFile.txt\""
-- test_show1 = assertEqual r1 (showT s1)

-- test_read = assertEqual s1 (readNoteT "243" . showT $ s1 )

s2 = makeRelFile  "testFile.txt" :: Path Rel File 
-- r2 = s1
-- test_make = assertEqual r2 s2

-- test_toP = assertEqual (p3a) (s2t . P.toFilePath $  s3)
-- n1 = "testFile.txt" :: FilePath 
-- p3 = P.parseRelFile n1 :: Maybe (P.Path Rel File )
-- s3 = path2internal s2 :: P.Path Rel File 
-- p3a = s2t . P.toFilePath . fromJustNote "sdsw" $ p3 

-- test_makeRelDir = assertEqual d4 f4
-- f4 = makeRelDir "Workspace"
-- d4 = Path Rel Dir "Workspace/" :: Path Rel Dir 
-- s4 = showT d4
-- test_RelDir = assertEqual d4 (readNoteT "RD" s4)

-- test_addDir = assertEqual r6  (addDir f4  n6)
-- n6 = makeRelFile . s2t $ n1 
r6 = makeRelFile "Workspace/testFile.txt"

-- test_filename = assertEqual n6 (getFileName r6)

-- data T1 = T1 {a1 :: Text
--             , p1 :: Path Abs Dir
--             , n1 :: Int
-- }

-- t1 = T1 