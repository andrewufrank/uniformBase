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
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PackageImports #-}

{-# OPTIONS_GHC -fno-warn-missing-methods #-}


module Uniform.Json_test where

import GHC.Generics
import         "HTF"  Test.Framework
import           Uniform.Zero
import           Data.Aeson
-- import Data.Map 
import           Uniform.Time
-- import Uniform.Strings 
import           Uniform.Json

-- import Test.Invariant
-- import Uniform.ByteString
--import qualified Data.ByteString as BS
--
-- testing for json records including time for show/read 

data Person =
  Person { firstName  :: !Text
         , lastName   :: !Text
         , age        :: Int
         , likesPizza :: Bool
           } deriving (Show, Read,Ord, Eq, Generic)
instance FromJSON Person
instance ToJSON Person 

data PersonKurz =
  PersonKurz { firstName  :: !Text
         , lastName   :: !Text
           } deriving (Show, Read,Ord, Eq, Generic)
instance FromJSON PersonKurz
instance ToJSON PersonKurz 

k1 = PersonKurz "paul" "kurz"
p1 = Person "peter" "meier" 30 True

-- ex1 = { "firstName"  : "Daniel"
--    , "lastName"   : "Díaz"
--    , "age"        :  24
--    , "likesPizza" :  true
--      }        
p2j = toJSON p1 
k2j = toJSON k1 

test_fromJSON = do 
        res <- runErr . callIO $ do 
                    let a =  fromJSON p2j 
                    a2 :: Person <- result1 a
                    return a2
        assertEqual (Right p1) res 

-- only solution, other hang without msg  
test_fromJSONk = do  -- fails with error msg
            -- important is callIO 
        res <- runErr . callIO $ do 
                    let a =  fromJSON k2j 
                    a2 :: Person <- result1 a
                    return a2
        assertEqual (Right p1) res 

-- test_fromJSONx = do 
--         res <- runErr  $ do 
--                     let a =  fromJSON p2j 
--                     a2 :: Person <- result1 a
--                     return a2
--         assertEqual (Right p1) res 

-- test_fromJSONxk = do  -- hangs
--         res <- runErr  $ do 
--                     let a =  fromJSON k2j 
--                     a2 :: Person <- result1 a
--                     return a2
--         assertEqual (Right p1) res 

-- the best solution -- but hangs if fields not present
-- test_fromJSONm = do 
--     res <- runErr $ 
--                 do 
--                     a2 <- fromJSONm k2j 
--                     -- a2 :: Person <- result1 a
--                     return a2
--     assertEqual (Right p1) res  -- expects fields not present

-- test_fromJSONm = do 
--     res <- runErr $ 
--                 do 
--                     a2 <- fromJSONm p2j 
--                     -- a2 :: Person <- result1 a
--                     return a2
--     assertEqual (Right p1) res 

test_fromJSONerrio = do 
    res <- runErr $ fromJSONerrio p2j 
                    -- a2 :: Person <- result1 a
                    -- return a2
    assertEqual (Right p1) res 
test_fromJSONerrioKurz = do 
    res <- runErr $ fromJSONerrio k2j 
                    -- a2 :: Person <- result1 a
                    -- return a2
    assertEqual (Right p1) res 


-- before 0.4 
-- data MetaRec = MetaRec {
--         -- text ::  Text  -- ^ naked filename -- not shown
--         -- , link :: Text -- ^ the url relative to dough dir
--         title :: Maybe Text -- ^ the title as shown
--         , date :: Maybe UTCTime -- read the time early one to find errors
--         , date2 ::  UTCTime -- read the time early one to find errors
--         , int1 :: Int -- to test assignement
--         } deriving (Generic, Eq, Ord, Show, Read)

-- instance Zeros MetaRec where
--     zero = MetaRec zero (Just year2000) year2000 zero
-- --instance FromJSON IndexEntry
-- instance ToJSON MetaRec
-- instance FromJSON MetaRec where

-- rec1 :: MetaRec
-- rec1 = zero { title = Just "rec1" } :: MetaRec
-- rec1shown :: String
-- rec1shown = show rec1 :: String

-- test_jsonTime :: IO ()
-- test_jsonTime = assertEqual rec1 (readNote "testJsonTime" rec1shown :: MetaRec)

-- test_encode :: IO ()
-- test_encode = assertEqual res1 $ encode rec1
-- -- res1 :: Data.ByteString.Lazy.Internal.ByteString
-- res1 =
--     "{\"int1\":0,\"date\":\"2000-01-01T00:00:00Z\",\"title\":\"rec1\",\"date2\":\"2000-01-01T00:00:00Z\"}"


-- -- encode rec1 

-- -- rec1json = Object (fromList [("date"
-- --                         ,String "2000-01-01T00:00:00Z")
-- --                         ,("title",String "rec1")
-- --                         ,("date2",String "2000-01-01T00:00:00Z")])
-- rec1json' :: Value
-- rec1json' = toJSON rec1

-- json2rec :: Either String MetaRec
-- json2rec = eitherDecode res1 :: Either String MetaRec
-- test_setInt :: IO ()
-- test_setInt =
--     assertEqual rec2json $ putAtKey "int1" (22 :: Integer) (toJSON rec0)

-- rec2json :: Value
-- rec2json = toJSON $ MetaRec zero (Just year2000) year2000 22
-- rec0 :: MetaRec
-- rec0 = MetaRec zero (Just year2000) year2000 zero

-- -- test_force = assertBool False 

-- test_fromJ = assertEqual (Just rec1) (fromJSONmaybe rec1json')
-- -- test_fromJx = assertEqual  Nothing (fromJSONmaybe rec1json')

-- val :: Value
-- val = object [
--   "boolean" .= True,
--   "numbers" .= [1,2,3::Int] ]  

-- test_val = assertEqual v1 (showT val)
-- v1 = "Object (fromList [(\"boolean\",Bool True),(\"numbers\",Array [Number 1.0,Number 2.0,Number 3.0])])"

-- test to see if a partial update with merge works

data RecFull = RecFull {boolean :: Bool 
                        , numbers :: [Int]
                        , firstInt :: Int
                        , secondReal :: Double 
                        , thirdString :: String 
                        , fourthText :: Text 
                        } 
                        deriving (Read, Show, Eq, Ord, Generic)

instance ToJSON RecFull 
instance FromJSON RecFull

 
data RecPart = RecPart {firstInt :: Int
                , fourthText :: Text }
                deriving (Read, Show, Eq, Ord, Generic)
                
instance ToJSON RecPart 
instance FromJSON RecPart

val7 = RecFull True [1,2,3] 1 2.22 "aaaa"  "áäö"
valOutput :: RecFull 
valOutput = RecFull {boolean = True, numbers = [1,2,3], firstInt = 1, secondReal = 2.22, thirdString = "aaaa", fourthText = "\225\228\246"}
valj = toJSON val7 

aDemo = do 
    p1 :: RecPart <-  (fromJSONm valj)
    putIOwords["the the part record from json", showT p1]

    let p2 = p1 {firstInt = 2, fourthText = "ascii"} :: RecPart
    let p2j = toJSON p2 

    let p3j = mergeLeftPref [p2j, valj]   -- first wins?
    putIOwords ["the merged p3", showT p3j]

    p3 :: RecFull <-  (fromJSONm p3j) 
    putIOwords ["the merged p3 from json", showT p3]

test_partialUpdate = do 
    res <- runErr $ do
        p1 :: RecPart <-  (fromJSONm valj)  -- get part
        let p2 = p1 {firstInt = 2, fourthText = "ascii"} :: RecPart
                -- updated part
        let p2j = toJSON p2
        let p3j = mergeLeftPref [p2j, valj] -- first wins
        p2check  :: RecPart <-  (fromJSONm p3j)

        return (p2, p2check) 
    let Right (p2, p2check) = res
    assertEqual p2 p2check  

test_partialUpdateReverse = do 
    res <- runErr $ do
        p1 :: RecPart <-  (fromJSONm valj)  -- get part
        let p2 = p1 {firstInt = 2, fourthText = "ascii"} :: RecPart
                -- updated part
        let p2j = toJSON p2
        let p3j = mergeRightPref [valj, p2j] -- first wins
        p2check  :: RecPart <-  (fromJSONm p3j)

        return (p2, p2check) 
    let Right (p2, p2check) = res
    assertEqual p2 p2check  