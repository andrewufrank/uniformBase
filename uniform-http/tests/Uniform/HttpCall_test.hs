
 -----------------------------------------------------------------------------
--
-- Module      :  Uniform.HttpCall
--
-- | using http simple to sparql queries and to create requests
-- part of uniform (to use only text
-- uses the newer http-conduit module
-- because teh old HTTP cannot do https

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

module Uniform.HttpCall_test where

import           Uniform.Error
--import           Uniform.Strings
--
--import     qualified      Network.HTTP.Simple          as Http
--import     qualified      Network.HTTP.Client          as Client
--import     qualified      Network.HTTP.Conduit         as Conduit
--import           Network.HTTP.Client.TLS
--import           Network.HTTP.Types.Status  (statusCode)
import Uniform.HttpCall

--import Data.Text (take)
import  Test.Framework
--import Uniform.HttpURI


makeHTTPgetrequestNoBody :: URItext -> Text -> Text -> Net.Request String
makeHTTPgetrequestNoBody uri argument text =
    Net.getRequest  $ concat [ t2s uri , t2s argument , Net.urlEncode . t2s $ text]




parseURLchecked ::  Text -> ErrIO NetURI.URI
parseURLchecked uri = do
    let  urx = NetURI.parseURI $ t2s uri
    case urx of
            Nothing ->  throwErrorT ["URLerror" , "not a proper url " ,  uri]
            Just uriEnc -> return uriEnc

urlEncodeVarsT:: [(Text,Text)] -> Text
urlEncodeVarsT = s2t . Net.urlEncodeVars . map (pair t2s)

urlEncode :: Text -> Text
urlEncode = s2t . Net.urlEncode . t2s

destTestFail = "127.0.0.1:9000"
destTest9001 = "http://127.0.0.1:9001"

test_parseURI = assertEqual "Just http://127.0.0.1:9001" (showT . NetURI.parseURI $ destTest9001)
test_parseURI_fail  = assertEqual "Nothing" (showT . NetURI.parseURI $ destTestFail)

uriTest = "http://127.0.0.1:9001/?annotators=tokenize%2Cssplit%2Cpos%2Clemma%2Cner%2Cparse&outputFormat=xml"

mimetypeTest = "test/application"
bodyTest = "This is a sentence."
res5 = "POST http://127.0.0.1:9001/?annotators=tokenize%2Cssplit%2Cpos%2Clemma%2Cner%2\
    \Cparse&outputFormat=xml HTTP/1.1\r\nAccept: */*\r\nContent-Length: 19\r\nContent-Type: \
    \test/application\r\n\r\n"


