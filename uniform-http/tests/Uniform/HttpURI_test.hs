
 -----------------------------------------------------------------------------
--
-- Module      :  Store.RDFstore.HttpCall
--
-- | using http simple to sparql queries and to create requests
-- part of uniform (to use only text

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.HttpURI_test where


import           Uniform.Error
--import           Uniform.Strings
--import Uniform.HttpCall
--import Uniform.HttpCallWithConduit
-- import qualified Network.URI as N
import           Test.Framework
import Uniform.HttpURI
import Uniform.Json 
import Uniform.Json (fromList)
-- import Data.Aeson as A
-- import Uniform.Zero
-- -- import qualified Network.URI as N

-- test for read show 
u1 = URI "http://nlp.gerastree.at:9001"

sg = showT u1 -- "URI http://nlp.gerastree.at:9001"
sgN = showNice u1 

test_readShow = assertEqual (u1) (readT sg)

-- test for to/from json 

test_json = assertEqual resJson (toJSON u1)
-- instance Zeros Value where zero = Null 

resJson :: Value 
resJson = String "http://nlp.gerastree.at:9001"

test_json2 = assertEqual (Just u1) (fromJSONmaybe resJson)

test_add2uri :: IO ()
test_add2uri = assertEqual u2 (addToURI u1 "testx") 

u2 = URI "http://nlp.gerastree.at:9001/testx"  

-- server read/show test

sv1 = mkServerURI2   "http://nlp.gerastree.at"
sv2 = mkServerURI2   "http://nlp.gerastree.at:9001"
test_sv1 = assertEqual sv1 (read . show $ sv1)
svShow = showT sv1 

test_addport = assertEqual sv2
            (addPort2ServerURI sv1 (mkPortNumber 9001))


forportTest :: ServerURI
forportTest = mkServerURI2 "http://127.0.0.1"

-- test_add2uri :: IO ()
-- test_add2uri = assertEqual "\"http://nlp.gerastree.at:9001/xtestx\""
--                         (showT $ addToURI destTest9001g "xtestx")
-- test_add2uri2 :: IO ()
-- test_add2uri2 = assertEqual "ServerURI {unServerURI = \"http://127.0.0.1:9000\"}"
--                         (showT $ addPort2ServerURI forportTest (PortNumber 9000))
-- destTestFailx :: Text
-- destTestOKx :: Text

-- destTestFailx = "127.0.0.1:9000" ::Text  -- missing http://
-- destTestOKx = "http://127.0.0.1:9001"
-- destTest9001g = makeAbsURI "http://nlp.gerastree.at:9001"
-- destTest9000e = makeAbsURI "http://nlp.gerastree.at:9000"

-- test_makeURIok = assertEqual "\"http://nlp.gerastree.at:9001\"" (showT   destTest9001g)
-- test_makeURIfail = do
--             res <- mustError "test failing uri construction"
--                                 $ return $ makeAbsURI  destTestFailx
--             assertBool (not res)
-- --test_makeURIfail2 = assertEqual "Nothing" (showT $ makeAbsURI  destTestFailx)

-- --test_makeURILineDesc_fail = assertEqual zero (makeURI "http://gerastree.at/test-t4#/TP[54/0002]L0013")
-- test_makeURILineDesc0 = assertEqual "http://gerastree.at/test-t4L0013" (makeURI "http://gerastree.at/test-t4L0013")
-- test_makeURILineDesc1 = assertEqual "http://gerastree.at/test-t4/TP/L0013" (makeURI "http://gerastree.at/test-t4/TP/L0013")
-- --test_makeURILineDesc2fail = assertEqual zero (makeURI "http%3A%2F%2Fgerastree.at%2Ftest-t4%2FTP%5B54%2F0002%5DL0013")

-- test_parseURI = assertEqual  (Just "htt://gerastree.at") (  parseURI "htt://gerastree.at")
-- test_parseURI1 = assertEqual  (Just "http://gerastree.at/test-t4L0013")
--                 (  parseURI "http://gerastree.at/test-t4L0013")
-- --test_parseURI2 = assertEqual  Nothing
-- --                (  parseURI "http://gerastree.at/test-t4#TP[54/0002]L0013")
-- --test_URL = assertEqual  resurl
-- --                (  s2url "http://gerastree.at/test-t4#/TP[54/0002]L0013")

-- resurl = URL "http%3A%2F%2Fgerastree.at%2Ftest-t4%23%2FTP%5B54%2F0002%5DL0013"

-- --test_parseURI3 = assertEqual  Nothing
-- --                (  parseURI . s2t . url2s $ resurl)

-- -- only the query part may be escaped, not the full string !!

-- --test_URL4 = assertEqual  resurl4
-- --                (  s2url "http://gerastree.at/test-t4#TP[54/0002]L0013")

-- resurl4 = URL "http%3A%2F%2Fgerastree.at%2Ftest-t4%23TP%5B54%2F0002%5DL0013"


-- --test_parseURI4 = assertEqual  Nothing
-- --                (  parseURI . s2t . url2s $ resurl4)
-- test_parseURI5 = assertEqual  ( Just "http://gerastree.at/test-t4/#TP54/0002/L0013")
--                 (  parseURI "http://gerastree.at/test-t4/#TP54/0002/L0013")
-- test_parseURI6 = assertEqual  ( Just "http://gerastree.at/test-t4/#TP5B54%2F0002%5D/L0013")
--                 (  parseURI "http://gerastree.at/test-t4/#TP5B54%2F0002%5D/L0013")


-- gerastree =  "http://gerastree.at/"
-- test_gerastreurl = assertEqual ( Just "http://gerastree.at/") (parseURI gerastree)

-- test_gerastree = assertEqual "http://gerastree.at/" (makeURI gerastree)
-- test_gerastreurl2 = assertEqual "http://gerastree.at/"  (makeURI "http://gerastree.at/")
-- gerastreeURI = makeURI  "http://gerastree.at/"

-- test_encode = assertEqual  (URL "test-t4%23TP%5B54%2F0002%5DL0013")
--                                     (  s2url . t2s $ "test-t4#TP[54/0002]L0013")
-- test_addTo2 = assertEqual "http://gerastree.at//test-t4%23TP%5B54%2F0002%5DL0013"
--                 (addToURI2 gerastreeURI (s2url . t2s $ "test-t4#TP[54/0002]L0013"))

-- test_encodeLineDesc = assertEqual resld (s2url ( "http://gerastree.at/test-t4/TP[54/0002]L0013" :: String))
-- resld = URL  "http%3A%2F%2Fgerastree.at%2Ftest-t4%2FTP%5B54%2F0002%5DL0013"

-- uriTest = "http://127.0.0.1:9001/?annotators=tokenize%2Cssplit%2\
--             \Cpos%2Clemma%2Cner%2Cparse&outputFormat=xml"

-- test_parseURI9 = assertEqual "Just \"http://127.0.0.1:9001\""
--                     (showT . parseURI $  destTestOKx)
-- --test_parseURI_fail  = assertEqual "Nothing" (showT . parseURI  $ destTestFailx)

-- --instance Read URI where
-- --        readsPrec i r =   maybe []  (\res -> [(res, rem)] ) $ parseURI x
-- --                where  [(x ::String , rem)] = readsPrec i r


-- test_s1 = assertEqual "http://nlp.gerastree.at:9001" (show destTest9001g)
-- test_r1 = assertEqual destTest9001g (read . show $ destTest9001g)



-- TODO 
readT = read . t2s 