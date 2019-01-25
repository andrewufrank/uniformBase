-----------------------------------------------------------------------------
--
-- Module      :  Filenames
-- Copyright   :  andrew u frank -
--
-- | the operations on filenames and extensions
--  uses the Path library
-- is a class except for the make


-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
--{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.Filenames_test  where
--
-- --import qualified Data.Text as T
-- import qualified System.Posix  as P (FileStatus)
-- --import qualified System.Directory as S
--
---- using uniform:
import           Uniform.Error hiding ((</>), (<.>))
-- import           Uniform.Strings     hiding ((</>), (<.>))
            -- (s2t, showT, t2s, removeChar, CharChains2 (..), Text)
--import Safe   -- todo error
--import Path   hiding ( (</>) ) -- should I hide the quasi quoters?
--import qualified Path   ((</>))
----import qualified          System.Posix.FilePath as P
--import Path.IO
--import  qualified         System.FilePath       as S -- prefered
--import  qualified         System.FilePath.Posix       as S -- prefered
--import  qualified         Filesystem.Path       as F -- prefered
-- not usable, has a different definition of FilePath

import Test.Framework
-- import Test.Invariant
import Uniform.Filenames


--
test_show = assertEqual "\"afile\"" (show g1)
test_read = assertEqual  g1 (read "\"afile\"")

test_read2 = assertEqual g1 (makeRelFile "afile")

--test_readrd = assertEqual  g1 (read "afile")
test_readaf = assertEqual  g3 (read "\"/somedir/more/afile.ext\"")
--test_readrf = assertEqual  g1 (read "afile")

testdir1 = makeAbsDir "/home/frank/test"
testfile1 = "file1.x" :: FilePath
testdir2 = "files" :: FilePath
test_addFilename = assertEqual "/home/frank/test/file1.x" (toFilePath $ addFileName testdir1 testfile1)
--test_addFilenameEmpty = assertEqual "" (toFilePath $ addFileName testdir1 (""::FilePath))
-- does fail

test_addDir = assertEqual "/home/frank/test/files/" (toFilePath $ addDir testdir1 testdir2)
test_addDirEmpty = assertEqual "/home/frank/test/" (toFilePath $ addDir testdir1 (""::FilePath))

--test_abs1 = assertEqual "" $ makeAbsDir  "file://home/frank/additionalSpace/DataBig/LitTest/test"

test_zeroAbsFile = assertEqual "/zero" (toFilePath (zero:: Path Abs File))
test_zeroAbsDir = assertEqual  "/" (toFilePath (zero:: Path Abs Dir))
test_zeroRelFile = assertEqual "zero" (toFilePath (zero:: Path Rel File))
test_zeroRelDir = assertEqual "./" (toFilePath (zero:: Path Rel Dir))

testname = "/home/frank/dir1/file.ext" :: FilePath
test_immediateParent = assertEqual "dir1" (getImmediateParentDir testname)
test_nakedFilename = assertEqual "file" (getNakedFileName testname)

testname2 = makeAbsFile testname


test_immediateParent2 = assertEqual "dir1" (getImmediateParentDir testname2)
test_nakedFilename2 = assertEqual "file" (getNakedFileName testname2)

--------
x1f = makeAbsFile f3 :: Path Abs File
x1t = showT x1f
x1s = t2s x1t

test_sp = assertEqual "\"/somedir/more/afile.ext\"" (show x1f)

test_sp1 = assertEqual ("/somedir/more/afile.ext"::String) (read x1s)

data S2 = S2 String String deriving (Show, Read,  Eq)
--instance Read S2 where
--    readsPrec i s = [(S2 a b, r)]
--        where
--            [(b, r)] = readsPrec i s2
--            [(a, s2)] = readsPrec i s

test_p = assertEqual [("DREI", " someMore")] (readsPrec 0 "\"DREI\" someMore" )

s2a = S2 "eins" "zwei"
s2as = show s2a   -- "S2 \"eins\" \"zwei\""

test_s2aa = assertEqual "S2 \"eins\" \"zwei\"" (s2as)
test_s2a = assertEqual s2a (read   "S2 \"eins\" \"zwei\"")

data Xt = Xt { p :: Path Abs File
                , q :: Text
                } deriving (Show, Read, Eq)

--instance Read (Path Abs File) where
--        readsPrec i r =   -- r ist  "/somedir/more/afile.ext", q = "f3"}
--                             [(makeAbsFile x, rem)] -- ", q = \"f3\"")]
--                where
--                    [(x ::String , rem)] = readsPrec i r

xt = Xt x1f "f3"
xt3 = Xt "/somedir/more/afile.ext" "f3"
xts = show xt

test_xt1 = do
        putIOwords ["xt1 - xts is:", s2t xts]
        putIOwords ["xt1 - show xt is:", showT xt]
        assertEqual xt  (read $ xts)

xt2 = Xt {p = "/somedir/more/afile.ext", q = "f3"}

--test_rp = do
--        putIOwords ["rp - f3 :", s2t  f3s]
--        assertEqual [(x1f, "")] (readsPrec 0 f3s :: [(Path Abs File, String)]  )
--
--test_r2 = assertEqual x1f (readNote "r2" f3s)
--f3s = show x1f
--
----instance Show (Path Abs File) where
----    show = toFilePath
--
--test_xt2 = do   -- ok
--        putIOwords ["xt2 - s:",   x1s]
--        putIOwords ["xt1 - show p . xt is:", showT . p $ xt]
--        assertEqual x1f  (readT $ x1s)
--
--test_rp2 = do  -- ok
--        putIOwords ["xt2 - x1s:",   x1s]
----        putIOwords ["xt1 - show p . xt is:", showT . p $ xt]
--        assertEqual [(x1f,"")]  (readsPrec 0 . t2s $ x1s)
--
--test_rp3 = do
--        putIOwords ["xt2 - x1ss:",  s2t x1ss]
----        putIOwords ["xt1 - show p . xt is:", showT . p $ xt]
--        assertEqual [(x1f,"")]  (readsPrec 0   $ x1ss)
--
--
--
--xt2r = readT xt2 :: Xt
--readT :: Read a => Text -> a
--readT s = readNote "readNotJust" . t2s $ s
--xt2 = showT xt  :: Text
--x1ss = t2s x1s ++ ", some text" :: String
------------------tests

-- rigerous filepath testing is difficult,
-- as many inputs are not leading to leagal path
f1 = "afile" :: FilePath
f0 = "" :: FilePath  -- not legal?
f2 = "afile.ext" :: FilePath
f3 = "/somedir/more/afile.ext"  :: FilePath
test_emptyExt = assertEqual "" (getExtension f1)
test_emptyExt0 = assertEqual "" (getExtension f0)
test_getExt = assertEqual "ext" (getExtension f2)
test_hasExt = assertBool $  hasExtension "ext" f2
test_hasExt2 = assertBool $  hasExtension "ext" f3
test_addExt = assertEqual ( f2) $  addExtension "ext" f1
test_removeExt = assertEqual f1 (removeExtension f2)
test_setExt = assertEqual ("afile.txt") (setExtension "txt" f2)


--prop_add_has_FP :: FilePath -> FilePath -> Bool
--prop_add_has_FP e f = if (isInfixOf' "." e) then True else prop_add_has e f
--prop_add_add_has_FP :: FilePath ->FilePath ->FilePath -> Bool
--prop_add_add_has_FP  =  prop_add_add_has
--prop_set_get_FP :: FilePath -> FilePath ->  Bool
--prop_set_get_FP  = prop_set_get

g1 = makeRelFile "afile" :: Path Rel File
--g0 = ""  -- not legal?
g2 = makeRelFile "afile.ext"
g3 = makeAbsFile "/somedir/more/afile.ext"
g4 = makeAbsFile "/somedir/more/afile.txt"
e1 = (Extension "ext")
--test_emptyExt_P = assertEqual (Extension "") (getExtension g1)
----test_emptyExt0 = assertEqual "" (getExtension f0)
--test_getExt_P = assertEqual e1 (getExtension g2)
--test_hasExt_P = assertBool $  hasExtension e1 g2
--test_hasExt2_P = assertBool $  hasExtension e1 g2
--test_addExt_P = assertEqual ( g2) $  addExtension e1 g1
--test_removeExt_P = assertEqual g1 (removeExtension g2)
--test_setExt_P = assertEqual ( g4) (setExtension (Extension "txt") g3)
--d1 = makeAbsDir "/somedir/more/dir"
--test_nakedDir = assertEqual "dir" (getNakedDir d1)

data TestRec = TestRec {f11:: Path Abs Dir} deriving (Show, Eq, Read)
inp1 = TestRec { f11 = "/home/frank/"}
inp2 = TestRec { f11 = makeAbsDir "/home/frank/"}
f11x = "/home/frank/" :: Path Abs Dir

--test_read1 = assertEqual inp1 (inp1)  -- must fail, reading a string into Path Abs
--                                      -- not permitted (should be detected when assign to inp1
--test_read12 = assertEqual "" (show inp1)
test_read22 = assertEqual "TestRec {f11 = \"/home/frank/\"}" (show inp2)

