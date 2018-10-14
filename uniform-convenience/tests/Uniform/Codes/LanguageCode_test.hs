-----------------------------------------------------------------------------
--
-- Module      :  complete the sentence in Defs0 mit lemma and a second PoS
-- Copyright   : af
--
-- conversin F -> G
-- is calling sentence by sentence for german lemmatization
-- if other lemmatization are necessary, then select different port

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables
--        , BangPatterns
            , UndecidableInstances
         #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--{-# OPTIONS_GHC -w #-}

module Uniform.Test.TestHarness_test   where

import           Test.Framework
import Uniform.Test.TestHarness
import Uniform.Zero
import Uniform.FileIO

test_3   = do

    test1File prgname "test.test1" "test.test2" id3
test_4   = do
    test1File prgname "test.test2" "test.test3" id4

id3 :: Abx -> Abx
id3 = const abx1

id4 :: Abx -> Abx
id4 = id

test_1   = do
    test1File prgname "test.test1a" "test.test2a" id1
test_2   = do
    test1File prgname "test.test2a" "test.test3a" id2

id1 :: Aby -> Aby
id1 = const aby1

id2 :: Aby -> Aby
id2 = id

prgname = "TestHarness" :: Text
abx1 = Abx as
fnt =   (makeAbsDir "/home/frank/Workspace8")
as = (map (\i -> A2 (showT i) (showT (i+100)) i) [1..10])
aby1   = Aby as fnt

data Abx = Abx [A2] deriving (Eq, Ord, Show, Read, Generic, Zeros)
data Aby = Aby [A2]  (Path Abs Dir)  deriving (Eq, Ord, Show, Read, Generic, Zeros)

data A2 = A2 Text Text Int
        deriving (Eq, Ord, Show, Read, Generic, Zeros)

--instance Zeros Abx where zero = Abx zero
--instance Zeros Aby where zero = Aby zero fnt

instance ShowTestHarness Abx where
instance ShowTestHarness Aby where
--    showTestH (Abx as) = "Abx " ++ showTestH as

instance ShowTestHarness A2 where
--    showTestH (A2 t1 t2  i ) = "A2 " ++ showTestH as
