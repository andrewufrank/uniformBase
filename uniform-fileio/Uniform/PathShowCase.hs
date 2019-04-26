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

module Uniform.PathShowCase

where

import Uniform.Strings
import Uniform.Error

import Path
import Data.Typeable
import Data.Data 



readsPrecForPath readOp prefix1 msg a1 = 
  case (p1,t1) of 
    ("",t2) -> errorT ["readPrexForPath", prefix1, msg, a1]
    (p2, t2) ->  -- prefix found 
                readOp 
  if (prefix1 `isPrefixOf'` a1 ) 
    then  [ (
      (fromJustNote (unwords["not a path ", msg, "input", show a1]) 
            . readOp
            . fromJustNote "prefix strip failed" 
            . stripPrefix' prefix1 $ a1) 
              , "")]
                    -- could need to see if anything is left 
    else error ("not a  prefix for " ++ msg ++ " input " ++ show a1)
  where (p1,t1) = splitWithPrefix prefix1 a1

instance  Read (Path Abs Dir) where 
  readsPrec _ = readsPrecForPath parseAbsDir prefixAbsDir "Abs Dir"

instance Read (Path Abs File) where 
  readsPrec _ = readsPrecForPath parseAbsFile prefixAbsFile "Abs File"
  
instance Read (Path Rel File) where
  readsPrec _ = readsPrecForPath parseRelFile prefixRelFile "Rel File"
instance Read (Path Rel Dir) where
  readsPrec _ = readsPrecForPath parseRelDir prefixRelDir "Rel Dir"
      
      
instance  {-# OVERLAPPING #-} Show (Path Abs Dir) where 
    show a = concat' [prefixAbsDir,  toFilePath a]
instance  {-# OVERLAPPING #-} Show (Path Abs File) where 
    show a = concat' [prefixAbsFile,  toFilePath a]
instance  {-# OVERLAPPING #-} Show (Path Rel File) where 
    show a = concat' [prefixRelFile,  toFilePath a]
instance  {-# OVERLAPPING #-} Show (Path Rel Dir) where 
    show a = concat' [prefixRelDir,  toFilePath a]

-- class ShowPrefix p  where 
--   getPrefix :: p -> String 
-- instance ShowPrefix (Path a b)

-- instance ShowPrefix (Path Abs Dir) where 
--    getPrefix a = prefixAbsDir 
-- instance ShowPrefix (Path Abs File) where 
--     getPrefix a = prefixAbsFile 
-- instance ShowPrefix (Path Rel File) where 
--     getPrefix a = prefixRelFile 
-- instance ShowPrefix (Path Rel Dir) where 
--    getPrefix a = prefixRelDir 

-- -- getPrefix (Path Abs File )
-- -- show (undefined::Abs) = "Abs"

-- instance (ShowPrefix (Path a b)) => Show (Path a b) where 
--   show a = concat' [getPrefix a, toFilePath a]

instance NiceStrings (Path a b) where 
  showNice = s2t . toFilePath 


toFilePathT = s2t . toFilePath

prefixAbsDir, prefixAbsFile, prefixRelDir, prefixRelFile :: String  
prefixAbsFile = "Path Abs File "
prefixAbsDir =  "Path Abs Dir "
prefixRelFile = "Path Rel File "
prefixRelDir = "Path Rel Dir "
---------------  
pathShowTest :: IO ()
pathShowTest = do
  putIOwords ["the path as string", showT p1]
  putIOwords ["the mayb pars ed path", showT p2]
  putIOwords ["the mayb pars ed path 2", showT p2]
  putIOwords ["the mayb pars ed path 2a", showT p2a]
  putIOwords ["just px, typed Path Abs Dir", showT p2x]
  putIOwords ["show r1 ", s2t r1]
  putIOwords ["prefix stripped", s2t r3]
  putIOwords ["parsed r3", showT r4 ]
  -- putIOwords ["read r2", showT r2]

r1 = show p2x :: String 
r2 = read r1 :: Path Abs Dir 
r3 = fromJustNote "prefix strip" 
              . stripPrefix' "Path Abs Dir " $ r1 :: String
r4 = parseAbsDir r3 :: Maybe (Path Abs Dir)

p1 = "/home/frank/testDir"
p1a = "/home/frank/testDir/"
p2 = parseAbsDir p1 :: Maybe (Path Abs Dir)
p2a = parseAbsDir p1a :: Maybe (Path Abs Dir)
p2x = fromJust p2 :: Path Abs Dir
-- p3 = show px :: String -- same as p1

f1 = "/home/frank/test.txt"
f2 =  parseAbsFile $ f1:: Maybe (Path Abs File)
f2a = fromJustNote "f2" f2
f3 = show f2a :: String 
f4 = read f3 :: Path Abs File 
 
g1 = "frank/test.txt"
g2 =  parseRelFile $ g1:: Maybe (Path Rel File)
g2a = fromJustNote "g2" g2
g3 = show g2a :: String 
g4 = read g3 :: Path Rel File 
 
h1 = "frank/test"
h2 =  parseRelDir $ h1:: Maybe (Path Rel Dir)
h2a = fromJustNote "h2" h2
h3 = show h2a :: String 
h4 = read h3 :: Path Rel Dir 

k1 = "/home/frank/test"
k2 =  parseAbsDir $ k1:: Maybe (Path Abs Dir)
k2a = fromJustNote "k2" k2
k3 = show k2a :: String 
k4 = read k3 :: Path Abs Dir 


l1 = "/home/frank/test/a.txt"
l2 =  parseAbsFile $ l1:: Maybe (Path Abs File)
l2a = fromJustNote "l2" l2
l3 = show l2a :: String 
l4 = read l3 :: Path Abs File 