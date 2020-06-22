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

import           Uniform.Error           hiding ( (<.>) )  -- (</>)
test_zero = assertEqual
  "DocRep {yam = Null, pan = Pandoc (Meta {unMeta = fromList []}) []}"
  (showT (zero :: DocRep))
test_readWrite = do
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

test_json = assertEqual res1 (encode rec1json)
rec1json = Object
  (fromList
    [ ("date" , String "2000-01-01T00:00:00Z")
    , ("title", String "rec1")
    , ("date2", String "2000-01-01T00:00:00Z")
    ]
  )

-- test_encode :: IO ()
-- test_encode = assertEqual res1 $ DocRep (encode rec1json) []
-- -- res1 :: Data.ByteString.Lazy.Internal.ByteString
res1 =
  "{\"date\":\"2000-01-01T00:00:00Z\",\"title\":\"rec1\",\"date2\":\"2000-01-01T00:00:00Z\"}"

-- test_setText = assertEqual rec2json
--         $ putAtKey "t1" (22::Integer) (toJSON rec0)
