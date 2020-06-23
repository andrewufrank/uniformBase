------------------------------------------------------------------------
--
-- Module      :  pandoc test
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
--{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.DocRep_test where
--
---- using uniform:
import           Test.Framework
import           Uniform.Pandoc
import           Uniform.DocRep
import Uniform.Json 
import Uniform.Test.TestHarness
import Text.Pandoc 
import           Uniform.Error           hiding ( (<.>) )  -- (</>)
import Uniform.Markdown_test

    
test_zero = assertEqual
    "DocRep {yam = Null, pan = Pandoc (Meta {unMeta = fromList []}) []}"
    (showT (zero :: DocRep))
test_readWriteDR = do
    res4 <- runErr $ do
        let
            pfn1 =
                makeAbsFile
                    "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort"
        let
            pfn2 =
                makeAbsFile
                    "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/someTextShort2"

        pan1 <- read8 pfn1 docRepFileType
        write8 pfn2 docRepFileType pan1

        pan2 <- read8 pfn2 docRepFileType
        return (pan1, pan2)
    putIOwords ["test_readWrite", "\n res1\n", showT res4, "\n"]
    let Right (target3, res3) = res4
    assertEqual target3 res3

-- this is for json (not aeson)
test_json = assertEqual res1 (encode rec1json)
rec1json = Object
    (fromList
        [ ("date" , String "2000-01-01T00:00:00Z")
        , ("title", String "rec1")
        , ("date2", String "2000-01-01T00:00:00Z")
        ]
    )
res1 =
    "{\"date\":\"2000-01-01T00:00:00Z\",\"title\":\"rec1\",\"date2\":\"2000-01-01T00:00:00Z\"}"

-- for aeson  see https://artyom.me/aeson
v1 = "Object (fromList [(\"boolean\",Bool True),(\"numbers\",Array [Number 1.0,Number 2.0,Number 3.0])])"
val :: Value
val = object [
  "boolean" .= True,
  "numbers" .= [1,2,3::Int] ]   

test_DR1 = assertEqual dr1t $ showT  dr1
dr1 = DocRep val zero
dr1t = "DocRep {yam = Object (fromList [(\"boolean\",Bool True),(\"numbers\",Array [Number 1.0,Number 2.0,Number 3.0])]), pan = Pandoc (Meta {unMeta = fromList []}) []}" :: Text

-- a11 =  Array [Number 1.0, Number 2.0, Number 3.0] :: Value    
-- test_encode :: IO ()
-- test_encode = assertEqual res2 $ DocRep (encode rec1json) zero
-- res2 = DocRep zero zero

-- -- res1 :: Data.ByteString.Lazy.Internal.ByteString

-- test_DRencode = assertEqual res2 $ 

test_setText = assertEqual rec2 
        $ showT $ putAtKey ("t1"::Text) (22::Integer) val
rec2 = "Object (fromList [(\"boolean\",Bool True),(\"t1\",Number 22.0),(\"numbers\",Array [Number 1.0,Number 2.0,Number 3.0])])" :: Text

test_set2dr1 = assertEqual rec3 $ showT . putAtKey ("a2"::Text) ("testa2"::Text) $ (dr1::DocRep)
rec3 = "DocRep {yam = Object (fromList [(\"a2\",String \"testa2\"),(\"boolean\",Bool True),(\"numbers\",Array [Number 1.0,Number 2.0,Number 3.0])]), pan = Pandoc (Meta {unMeta = fromList []}) []}"::Text

dr3 = putAtKey ("a2"::Text) ("testa2"::Text) $ (dr1::DocRep)
val3 = [object [
  "boolean2" .= False,
  "numbers" .= [4,44::Int] ]
  , object [
  "b4" .= ("b4test"::Text),
  "numbs" .= [66,55,44::Int] ]  ]
test_merge1 = assertEqual rec4 $ showT $ mergeAll dr3 val3
rec4 = "DocRep {yam = Object (fromList [(\"a2\",String \"testa2\"),(\"boolean\",Bool True),(\"numbs\",Array [Number 66.0,Number 55.0,Number 44.0]),(\"numbers\",Array [Number 4.0,Number 44.0]),(\"boolean2\",Bool False),(\"b4\",String \"b4test\")]), pan = Pandoc (Meta {unMeta = fromList []}) []}"::Text 

test_dr2texsnipShort = testVar0FileIO "uniform-DocRep" 
        shortFile
        "test_dr2texsnipShort" docRep2texsnipTest 
test_dr2texsnipReg = testVar0FileIO "uniform-DocRep" 
        regFile
        "test_dr2texsnipReg" docRep2texsnipTest 
test_dr2texsnipComplex = testVar0FileIO "uniform-DocRep" 
        complexFile
        "test_dr2texsnipComplex" docRep2texsnipTest 
test_dr2texsnipWithRef = testVar0FileIO "uniform-DocRep" 
        withRef
        "test_dr2texsnipWithRef" docRep2texsnipTest 

-- withRef = makeAbsFile "/home/frank/Workspace8/uniform/uniform-pandoc/tests/data/withRef.md"

docRep2texsnipTest :: Path Abs File -> ErrIO TexSnip
docRep2texsnipTest drfn  = do       
    dr1 :: DocRep <- read8 drfn docRepFileType 
    res1 :: TexSnip <-  docRep2texsnip  dr1 
    write8 drfn texSnipFileType res1
    return res1


test_dr2drop2htmlShort = testVar0FileIO "uniform-DocRep" 
        shortFile
        "test_dr2drop2htmlShort" docRep2htmlTest 
test_dr2drop2htmlReg = testVar0FileIO "uniform-DocRep" 
        regFile
        "test_dr2drop2htmlReg" docRep2htmlTest 
test_dr2drop2htmlComplex = testVar0FileIO "uniform-DocRep" 
        complexFile
        "test_dr2drop2htmlComplex" docRep2htmlTest 
test_dr2drop2htmlWithRef = testVar0FileIO "uniform-DocRep" 
        withRef
        "test_dr2drop2htmlWithRef" docRep2htmlTest 


docRep2htmlTest :: Path Abs File -> ErrIO HTMLout
docRep2htmlTest drfp = do 
    dr1 <- read8 drfp docRepFileType 
    h1 <- docRep2html dr1
    write8 drfp htmloutFileType h1
    return h1 

instance ShowTestHarness TexSnip
instance ShowTestHarness HTMLout