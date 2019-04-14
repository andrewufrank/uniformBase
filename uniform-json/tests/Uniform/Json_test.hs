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
-- import Data.Map 
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

test_encode = assertEqual res1 $ encode rec1
res1 =   
   "{\"date\":\"2000-01-01T00:00:00Z\",\"title\":\"rec1\",\"date2\":\"2000-01-01T00:00:00Z\"}"
-- encode rec1 

-- rec1json = Object (fromList [("date"
--                         ,String "2000-01-01T00:00:00Z")
--                         ,("title",String "rec1")
--                         ,("date2",String "2000-01-01T00:00:00Z")])

rec1json' = toJSON rec1 

json2rec = eitherDecode res1 :: Either String MetaRec 
-- test_2 = assertEqual (Right "") ( toJSON res1)

--- now for yaml 
