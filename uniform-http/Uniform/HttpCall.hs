
 -----------------------------------------------------------------------------
--
-- Module      :  Uniform.HttpCall
--
-- | using http simple to sparql queries and to create requests
-- part of uniform (to use only text
-- uses the newer http-conduit module
-- because teh old HTTP cannot do https

-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables, DeriveGeneric, DeriveAnyClass,
  RecordWildCards #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
-- {-# OPTIONS_GHC -fno-warn-missing-methods #-}


module Uniform.HttpCall (module Uniform.HttpCall
    , module Uniform.Error
    , mkServerURI, ServerURI
            )  where

import           Uniform.Error
import     qualified      Network.HTTP.Simple          as Http
import     qualified      Network.HTTP.Conduit         as Conduit

import Data.Text (take)
import Uniform.HttpURI
import GHC.Generics hiding (S)

makeRequest :: URI -> ErrIO Conduit.Request
makeRequest dest = Http.parseRequest . t2s . uriT $ dest


callHTTP8get :: Bool -> ServerURI  -> ErrIO  Text
-- call the http-conduit simple for a get
-- see https://haskell-lang.org/library/http-client
callHTTP8get debug (ServerURI dest) = do
    request <- makeRequest dest
    response <- callIO $  Http.httpLBS request
    when debug $ putIOwords ["The status code was: " ,
               showT (Http.getResponseStatusCode response)]
    when debug $ putIOwords [showT (Http.getResponseHeader "Content-Type" response)]
--    L8.putStrLn $ getResponseBody response
    let res = bb2t . bl2b . Http.getResponseBody $ response :: Text
    -- stops if not an UTF8 encoded text
    when debug $ putIOwords ["callHTTP8get response: ", res]
    return res

callHTTP10post :: Bool -> AppType -> ServerURI -> HttpPath -> LazyByteString
                    -> HttpQueryParams -> TimeOutSec -> ErrIO Text
-- post a body to the  url given as a type given
--application/sparql-update
-- timeout in seconds - will be converted, nothing gives default
    -- URI not text for destination
callHTTP10post debug (AppType apptype) (ServerURI dest) (HttpPath path)
                     txt vars (TimeOutSec timeout) = do
    req1 <- makeRequest dest
--    let length = lengthChar . b2s . bl2b $ txt
    let req2 = Http.setRequestBodyLBS txt -- (b2bl . t2b $ txt)
                $ Http.setRequestHeader "Content-Type" [t2b apptype]
                $ Http.setRequestMethod "POST"
                $ Http.setRequestPath (t2b path)
                $ Http.setRequestQueryString (map formatQuery
                        . unHttpQueryParams $ vars)
                req1
                    {Conduit.responseTimeout =
                            maybe Conduit.responseTimeoutNone
                                    (Conduit.responseTimeoutMicro . (1000000 *))
                                    timeout
                    }
    when debug $ putIOwords ["callHTTP10post" , showT req2 ]
--            "text length"
--                    , showT length]
    res <- callIO $ do
              Http.httpLBS req2
            `catchError` \e -> do
                     putIOwords ["callHTTP10post  error caught 3", showT e
                            , "\n should not occur - caught by callIO ??"
                            , "\n note hint: replace localhost by 127.0.0.1"
                            ,  "\n", showT req2]
                     fail . unwords $  [ "callHTTP10post httperror 3", show e]
                                             -- is in the IO monad, not ErrIO
    let statusCode = Http.getResponseStatusCode res
--    when debug $
    putIOwords ["callHTTP10post The status code was: ", showT statusCode]
    when debug $ putIOwords [showT (Http.getResponseHeader "Content-Type" res)]
    let res2 = bb2t . bl2b . Http.getResponseBody $ res :: Text
    -- stops if not an UTF8 encoded text
    return res2

-- -- TODO merge the post7 and post9
-- -- post7 has a query paramter with
-- makeHttpPost7 :: Bool ->  URI -> Text -> HttpQueryParams
-- -> Text -> Text ->  ErrIO Text
-- -- post a body to the  url given as a type given
-- --application/sparql-update
-- -- path is query .. or something which is type,value pairs
-- -- is not used anymore?
-- makeHttpPost7 debug dest path query appType txt = do
--     callHTTP10post debug appType ( dest) path (b2bl . t2b $ txt) query (Just 300)



formatQuery :: (Text, Maybe Text) -> (ByteString, Maybe ByteString)
formatQuery (a, mb) = (t2b a, fmap t2b mb)
--
-- makeHttpPost7x  :: Bool ->  URI -> Text ->
--  HttpQueryParams -> Text -> Text ->  ErrIO Text
-- -- post a body to the  url given as a type given
-- --application/sparql-update
-- -- path is query .. or something which is type,value pairs
-- makeHttpPost7x  debug dest path vars appType txt = do
--     req1 <- makeRequest dest
--     let length = lengthChar txt
--     let req2 = Http.setRequestBodyLBS  (b2bl . t2b $ txt)
--                 $ Http.setRequestHeader "Content-Type" [t2b appType]
--                 $ Http.setRequestMethod "POST"
--                 $ Http.setRequestPath (t2b path)
--                 $ Http.setRequestQueryString
--                             (map formatQuery . unHttpQueryParams $ vars)
-- --                $ Conduit.ResponseTimeout 300000 -- msecs
--                 req1
--                     {Conduit.responseTimeout = Conduit.responseTimeoutMicro 300000000}
-- ----            }
--     when debug $ putIOwords ["makeHttpPost7", showT req2, "text length", showT length]
--     res <- callIO $
--         do
--                  Http.httpLBS req2
--             `catchError` \e -> do
--                      putIOwords ["makeHttpPost7  error caught 3", showT e
--                             , "\n should not occur - caught by callIO ??"
--                             , "\n note hint: replace localhost by 127.0.0.1"
--                             ,  "\n", showT req2]
--                      fail . unwords $  [ "makeHttpPost7 httperror 3", show e]
--                                          -- is in the IO monad, not ErrIO


--     let statusCode = Http.getResponseStatusCode res
--     when debug $ putIOwords ["makeHttpPost7 The status code was: ", showT statusCode]
--     when debug $ putIOwords ["\t", showT (Http.getResponseHeader "Content-Type" res)]
--     let res2 = bb2t . bl2b . Http.getResponseBody $ res :: Text
--     -- stops if not an UTF8 encoded text
-- --    when True $ putIOwords ["makeHttpPost7 response: ", res2]
--     return res2


