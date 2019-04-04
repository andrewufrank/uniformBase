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
{-# LANGUAGE DeriveGeneric    #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}


module Uniform.Json_test where


import Test.Framework
import Uniform.Zero
import           Data.Aeson
import Uniform.Time 
import Uniform.Strings 
 
-- import Test.Invariant
-- import Uniform.ByteString
--import qualified Data.ByteString as BS
--
-- testing for json records including time for show/read 

data MetaRec = MetaRec {
        -- text ::  Text  -- ^ naked filename -- not shown
        -- , link :: Text -- ^ the url relative to dough dir
        title :: Maybe Text -- ^ the title as shown
        , date :: Maybe UTCTime -- read the time early one to find errors
        , date2 ::  UTCTime -- read the time early one to find errors
        } deriving (Generic, Eq, Ord, Show, Read)

instance Zeros MetaRec where
  zero = MetaRec zero   (Just year2000) year2000
--instance FromJSON IndexEntry
instance ToJSON MetaRec
instance FromJSON MetaRec where

rec1 = zero {title = Just "rec1"} :: MetaRec 
rec1shown = show rec1 :: String 

test_jsonTime = assertEqual rec1 
        (readNote "testJsonTime" rec1shown :: MetaRec)

-- test_forceoutput = assertBool False 

