
 -----------------------------------------------------------------------------
--
-- Module      :  Store.RDFstore.HttpCall
--
-- | using http simple to sparql queries and to create requests
-- part of uniform (to use only text
-- wraps URI in URI

-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE StandaloneDeriving
--    , GeneralizedNewtypeDeriving
    , DeriveGeneric
    , DeriveAnyClass
      #-}
 {-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Uniform.HttpURI (
        -- TimeOutSec, mkTimeOut, mkTimeOutDefault
        -- , URI, HttpQueryParams
    module Uniform.HttpURI
    , module Uniform.Zero
    , module Uniform.Strings
--    , module N.Network.URI
    -- , uriT
            )  where


import qualified Network.URI as N
-- import  Network.URI (URI(..)) 
-- URI is a newtype with URI as a wrapper
import           Uniform.Error (errorT)
import           Uniform.Json
import           Uniform.ListForm -- (IsString (..), (</>), (<.>))
import           Uniform.Strings 
import           Uniform.Strings.Infix ((</>), (<.>))

import           Uniform.Zero
--import qualified   Network.URI.Encode as N2

-- a server URI (not including the port, but absolute)
newtype ServerURI = ServerURI {unServerURI :: URI}
                deriving (Show, Read, Eq, Ord, Generic, Zeros, Semigroup, Monoid
--                        , ListForms
                        )
--deriving  -- <> and mempty missing for Semigroup

instance ToJSON ServerURI 
instance ToJSON N.URI
-- instance ToJSON N.URI  -- not possible, issue Auth
instance ToJSON N.URIAuth

deriving instance Generic N.URIAuth 

instance (Zeros ServerURI, Zeros (LF ServerURI)) => ListForms ServerURI
    where
    type LF ServerURI = Text
    mkOne = mkServerURI  -- = ServerURI . makeAbsURI
    appendTwo a b = ServerURI $ appendTwo (unServerURI a)  (unServerURI b)

--instance ListForms URI where
--    type LF URI = Text
--    mkOne = makeURI
--    appendTwo a b = makeURI $ appendTwo  (showT a) (showT b)

mkServerURI :: Text -> ServerURI        -- useful, because it is typed!
mkServerURI = ServerURI . makeURI  -- check for absolute uri?

-- | a type for the application path when calling Http
-- after the URI till the ? (starts with /)
newtype HttpPath = HttpPath Text
    deriving (Show, Read, Eq, Ord, Generic, Zeros)
mkHttpPath :: Text -> HttpPath
mkHttpPath = HttpPath    -- could check for acceptance here?

-- | a timeout in seconds
newtype TimeOutSec = TimeOutSec (Maybe Int)
    deriving (Eq, Ord, Show, Read, Generic, Zeros)
mkTimeOutSec :: Int -> TimeOutSec
mkTimeOutSec i = TimeOutSec (Just i)
mkTimeOutDefault = TimeOutSec Nothing

instance NiceStrings TimeOutSec where
    shownice (TimeOutSec (Just i)) = unwords' ["TimeOut", shownice i, "sec"]
    shownice (TimeOutSec Nothing) =   "TimeOut default"

-- | a special type for the app type argumetn
newtype AppType = AppType Text
    deriving (Eq, Ord, Show, Read, Generic, Zeros)
mkAppType = AppType

-- | the type for the paramter key - value pairs, comes after the ?
unHttpQueryParams :: HttpQueryParams -> [(Text, Maybe Text)]
mkHttpQueryParams :: [(Text, Maybe Text)] -> HttpQueryParams
newtype HttpQueryParams = HttpQueryParams [(Text, Maybe Text)]
    deriving (Show, Read, Eq, Generic, Zeros, Semigroup, Monoid)
unHttpQueryParams (HttpQueryParams p) = p
mkHttpQueryParams = HttpQueryParams
--instance Zeros HttpQueryParams where zero = HttpQueryParams []
-- unclear why automatic derivation does not work

instance   ListForms HttpQueryParams where
        type LF HttpQueryParams = (Text, Maybe Text)
        mkOne a = HttpQueryParams [a]
        appendTwo = (<>)


combineHttpQueryParams :: HttpQueryParams -> HttpQueryParams -> HttpQueryParams
combineHttpQueryParams p1 p2 = p1 <> p2
--    HttpQueryParams (p11 ++ p22)
--        where   p11 = unHttpQueryParams p1
--                p22 = unHttpQueryParams p2

newtype URI = URI N.URI  deriving (Eq, Ord, Generic,   Semigroup, Monoid)
-- show and read is separately instantiated
-- zeros not available for N.URI

un2 (URI u) = u   -- to remove the newtype level
-- instance Zeros URI where
    -- zero = makeURI "http://zero.zero"  -- there is no obvious zero here
instance Zeros URI where zero = URI N.nullURI 
instance ToJSON URI 
instance FromJSON URI
instance FromJSON N.URI
instance FromJSON N.URIAuth

instance ListForms URI where
    type LF URI = Text
    mkOne = makeURI  -- do not test here for validity, because it is used for appendTwo
    appendTwo a b = makeURI $ appendTwo  (uriT a) (uriT b)

parseURI :: Text -> Maybe URI
parseURI u = maybe (errorT ["parseURI in Uniform.HttpURI not acceptable string \n", u, "END of string"])
                (Just . URI)
                (N.parseURI  . t2s $ u )
--                fmap URI . N.parseURI . t2s $ t

parseAbsoluteURI :: Text -> Maybe URI
parseAbsoluteURI u = maybe (errorT ["parseAbsoluteURI in Uniform.HttpURI not acceptable string \n", u, "END of string"])
                (Just . URI)
                (N.parseAbsoluteURI  . t2s $ u )
--                fmap URI . N.parseAbsoluteURI . t2s $ t

makeAbsURI :: Text -> URI
makeAbsURI u = -- error "absfr"
    maybe (errorT ["makeAbsURI in Uniform.HttpURI not acceptable string \n", u, "END of string"])
                id
                (parseAbsoluteURI  u :: Maybe URI)
--    URI $ maybe (errorT ["makeAbsURI in Uniform.HttpURI", u])
--                id
--                (N.parseAbsoluteURI . t2s   $ u)
makeURI :: Text -> URI
makeURI u = -- error "sdafsfs"
    maybe (errorT ["makeURI in Uniform.HttpURI not acceptable string \n", u, "END of string"])
                id
                (parseURI  u :: Maybe URI)
-- alternative code: makeURI2 = fromMaybe zero . parseURI


addToURI :: URI -> Text -> URI   -- an url encoded string (use s2url or t2url)
-- add a text at end to an URI
addToURI u t =    --appendOne u t --
            makeURI $ (uriT u) </> (s2t . unURL . s2url . t2s $  t)

addToURI2 :: URI -> URL -> URI   -- an url encoded string (use s2url or t2url)
-- add a text at end to an URI
addToURI2 u t =    --appendOne u t --
            makeURI $ (uriT u) </> (s2t . unURL $ t)

newtype PortNumber = PortNumber Int
    deriving (Eq, Ord, Show, Read, Generic, Zeros)
mkPortNumber i = if i > 0 && i < 64000 then  PortNumber i
                    else errorT ["PortNumber out of range", showT i]
unPortNumber (PortNumber i) = i

addPort2ServerURI :: ServerURI -> PortNumber -> ServerURI
addPort2ServerURI u p = mkOne $ appendTwo  (uriT . unServerURI $ u)
            (":" <> (showT . unPortNumber $ p))
--addPort2ServerURI (ServerURI u) (PortNumber i) = mkServerURI (uriT u <:> showT i)

uriT :: URI -> Text
-- ^ convert an uri to a text (but not a show instance with "")
uriT = s2t . uriS

uriS :: URI -> String
uriS u =  N.uriToString defaultUserInfoMap (un2 u) $ ""
-- to filter out the password, if any in a URI

-- copied
defaultUserInfoMap :: String -> String
defaultUserInfoMap uinf = user ++ newpass
    where
        (user,pass) = break (==':') uinf
        newpass     = if null pass || (pass == "@")
                                   || (pass == ":@")
                        then pass
                        else ":...@"

instance IsString URI where
    fromString = read . show

instance Show URI where
    showsPrec _ s s2 = (show $ uriS s )++ s2

instance Read URI where
        readsPrec i r =  maybe []  (\res -> [(URI res, rem1)] ) $ N.parseURI x
                where  [(x ::String , rem1)] = readsPrec i r



