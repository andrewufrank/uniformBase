{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Uniform.PathWrapper
    (module Uniform.PathWrapper
    , P.File, P.Dir, P.Abs, P.Rel
    )

where

import Uniform.Strings
import Uniform.Error

import qualified Path as P
import Path (File, Dir, Abs, Rel)
-- import Data.Typeable
-- import Data.Data 


data Path a d = Path a d Text
    deriving (Show, Read, Ord, Eq, Generic)

path2internal (Path Rel File fn) = fromJustNote ("makeRelFile " ++ fn) $ P.parseRelFile fn

---------------  
-- pathShowTest :: IO ()
-- pathShowTest = do
--   putIOwords ["the path as string", showT p1]
--   putIOwords ["the mayb pars ed path", showT p2]
--   putIOwords ["the mayb pars ed path 2", showT p2]
--   putIOwords ["the mayb pars ed path 2a", showT p2a]
--   putIOwords ["just px, typed Path Abs Dir", showT p2x]
--   putIOwords ["show r1 ", s2t r1]
--   putIOwords ["prefix stripped", s2t r3]
--   putIOwords ["parsed r3", showT r4 ]
--   -- putIOwords ["read r2", showT r2]

-- r1 = show p2x :: String 
-- r2 = read r1 :: Path Abs Dir 
-- r3 = fromJustNote "prefix strip" 
--               . stripPrefix' "Path Abs Dir " $ r1 :: String
-- r4 = parseAbsDir r3 :: Maybe (Path Abs Dir)

-- p1 = "/home/frank/testDir"
-- p1a = "/home/frank/testDir/"
-- p2 = parseAbsDir p1 :: Maybe (Path Abs Dir)
-- p2a = parseAbsDir p1a :: Maybe (Path Abs Dir)
-- p2x = fromJust p2 :: Path Abs Dir
-- -- p3 = show px :: String -- same as p1

-- f1 = "/home/frank/test.txt"
-- f2 =  parseAbsFile $ f1:: Maybe (Path Abs File)
-- f2a = fromJustNote "f2" f2
-- f3 = show f2a :: String 
-- f4 = read f3 :: Path Abs File 
 
-- g1 = "frank/test.txt"
-- g2 =  parseRelFile $ g1:: Maybe (Path Rel File)
-- g2a = fromJustNote "g2" g2
-- g3 = show g2a :: String 
-- g4 = read g3 :: Path Rel File 
 
-- h1 = "frank/test"
-- h2 =  parseRelDir $ h1:: Maybe (Path Rel Dir)
-- h2a = fromJustNote "h2" h2
-- h3 = show h2a :: String 
-- h4 = read h3 :: Path Rel Dir 

-- k1 = "/home/frank/test"
-- k2 =  parseAbsDir $ k1:: Maybe (Path Abs Dir)
-- k2a = fromJustNote "k2" k2
-- k3 = show k2a :: String 
-- k4 = read k3 :: Path Abs Dir 


-- l1 = "/home/frank/test/a.txt"
-- l2 =  parseAbsFile $ l1:: Maybe (Path Abs File)
-- l2a = fromJustNote "l2" l2
-- l3 = show l2a :: String 
-- l4 = read l3 :: Path Abs File 