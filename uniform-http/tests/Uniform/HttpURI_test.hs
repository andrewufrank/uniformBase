
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
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.HttpURI_test where


import           Uniform.Error
--import           Uniform.Strings
--import Uniform.HttpCall
--import Uniform.HttpCallWithConduit
--import Network.URI
import           Test.Framework
import Uniform.HttpURI

--

test_add2uri = assertEqual "\"http://nlp.gerastree.at:9001/xtestx\""
                        (showT $ addToURI destTest9001g "xtestx")
test_add2uri2 = assertEqual "ServerURI {unServerURI = \"http://127.0.0.1:9000\"}"
                        (showT $ addPort2ServerURI forportTest (PortNumber 9000))


destTestFailx = "127.0.0.1:9000" ::Text  -- missing http://
destTestOKx = "http://127.0.0.1:9001"
destTest9001g = makeAbsURI "http://nlp.gerastree.at:9001"
destTest9000e = makeAbsURI "http://nlp.gerastree.at:9000"

test_makeURIok = assertEqual "\"http://nlp.gerastree.at:9001\"" (showT   destTest9001g)
test_makeURIfail = do
            res <- mustError "test failing uri construction"
                                $ return $ makeAbsURI  destTestFailx
            assertBool (not res)
--test_makeURIfail2 = assertEqual "Nothing" (showT $ makeAbsURI  destTestFailx)


test_addport = assertEqual
            "ServerURI {unServerURI = \"http://127.0.0.1:9001\"}"
            (showT $ addPort2ServerURI forportTest (mkPortNumber 9001))


forportTest :: ServerURI
forportTest = mkServerURI "http://127.0.0.1"

uriTest = "http://127.0.0.1:9001/?annotators=tokenize%2Cssplit%2\
            \Cpos%2Clemma%2Cner%2Cparse&outputFormat=xml"

test_parseURI = assertEqual "Just \"http://127.0.0.1:9001\""
                    (showT . parseURI $  destTestOKx)
test_parseURI_fail  = assertEqual "Nothing" (showT . parseURI  $ destTestFailx)

--instance Read URI where
--        readsPrec i r =   maybe []  (\res -> [(res, rem)] ) $ parseURI x
--                where  [(x ::String , rem)] = readsPrec i r


test_s1 = assertEqual "\"http://nlp.gerastree.at:9001\"" (show destTest9001g)
test_r1 = assertEqual destTest9001g (read . show $ destTest9001g)
