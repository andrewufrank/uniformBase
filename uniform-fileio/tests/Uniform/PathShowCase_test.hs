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
import  Uniform.PathShowCase
import qualified Path as P
import  Path
    (parseRelDir, parseAbsDir, parseRelFile, parseAbsFile)
import Control.Exception
import Uniform.Filenames
-- import Uniform.Test.TestHarness
-- cannot be used (cycle fileio - convenience)



data T1 = T1 {a11 :: Text
            , p11 :: Path Rel File
            , n11 :: Int
                } deriving (Show, Read, Eq)

t1 = T1 "some text" s2  37
st1 = showT t1
rt1 = read. t2s $ st1 :: T1

t2 = T1 "anoher long, atest text" r6  2009
rt2 = read . show $ t2 :: T1

test_sc1 = assertEqual t1 rt1
test_sc2 = assertEqual t2 rt2

data T2 = T2 {a12 :: Text
            , p12 :: Path Rel File
                } deriving (Show, Read, Eq)

t22 = T2 "some interesting text" s2
test_sc3 = assertEqual (t22) (read . show $ t22)

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

---------------
pathShowTest :: IO ()
pathShowTest = do
  putIOwords ["the path as string", showT p1]
  putIOwords ["the mayb pars ed path", showT p2]
  putIOwords ["the mayb pars ed path 2", showT p2]
  putIOwords ["the mayb pars ed path 2a", showT p2a]
  putIOwords ["just px, typed Path Abs Dir", showT p2x]
  putIOwords ["show r1 ", s2t r1]
  putIOwords ["prefix stripped", s2t r3]
  putIOwords ["parsed r3", showT r4 ]
  -- putIOwords ["read r2", showT r2]

r1 = show p2x :: String
r2 = read r1 :: Path Abs Dir
r3 = fromJustNote "prefix strip"
              . stripPrefix' "Path Abs Dir " $ r1 :: String
r4 = parseAbsDir r3 :: Maybe (Path Abs Dir)

p1 = "/home/frank/testDir"
p1a = "/home/frank/testDir/"
p2 = parseAbsDir p1 :: Maybe (Path Abs Dir)
p2a = parseAbsDir p1a :: Maybe (Path Abs Dir)
p2x = fromJust p2 :: Path Abs Dir
-- p3 = show px :: String -- same as p1

f11 = "/home/frank/test.txt"
f21 =  parseAbsFile $ f11:: Maybe (Path Abs File)
f2a = fromJustNote "f2" f21
f31 = show f2a :: String
f41 = read f31 :: Path Abs File

g1 = "frank/test.txt"
g2 =  parseRelFile $ g1:: Maybe (Path Rel File)
g2a = fromJustNote "g2" g2
g3 = show g2a :: String
g4 = read g3 :: Path Rel File

h1 = "frank/test"
h2 =  parseRelDir $ h1:: Maybe (Path Rel Dir)
h2a = fromJustNote "h2" h2
h3 = show h2a :: String
h4 = read h3 :: Path Rel Dir

k1 = "/home/frank/test"
k2 =  parseAbsDir $ k1:: Maybe (Path Abs Dir)
k2a = fromJustNote "k2" k2
k3 = show k2a :: String
k4 = read k3 :: Path Abs Dir


l1 = "/home/frank/test/a.txt"
l2 =  parseAbsFile $ l1:: Maybe (Path Abs File)
l2a = fromJustNote "l2" l2
l3 = show l2a :: String
l4 = read l3 :: Path Abs File

