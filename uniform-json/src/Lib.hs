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

module Lib
    ( someFunc
    )
where

import           Data.Aeson
import           Data.HashMap.Strict     hiding ( map
                                                , null
                                                )
-- import Data.List  -- .GroupBy
import           GHC.Exts                       ( groupWith )
-- import           Data.Text                      ( Text(..) )
import Uniform.Strings 
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.List

someFunc :: IO ()
someFunc = do
    putIOwords ["someFunc", showT hg]

t1 = "A" :: Text
t2 = "1" :: Text
t3 = "2" :: Text

type Pair = (Text, Value)

v1 = ["name" .= t1, "age" .= t2] :: [Pair]
v2 = ["name" .= t1, "age" .= t3] :: [Pair]

v3 = ["x1" .= t3] :: [Pair]
v4 = ["name" .= t2]
-- unObject :: Value -> [Pair]
-- unObject (Object o) =  o 

h1 = fromList v1
h2 = fromList v2 :: HashMap Text Value

h12 = concat [v1, v2, v3, v4]
hg = groupWith fst h12 :: [[Pair]]

mergePairs :: [Pair] -> [(Text, Value)]
-- ^ a group with the same key is converted to a set of values
mergePairs as = map mergeOneGroup . groupWith fst $ as
  where
    mergeOneGroup :: [Pair] -> (Text, Value)
    mergeOneGroup as = if null as then error "null input" else (a .= bs)
      where 
        a  = fst . head $ as
        bs = nub $ map snd as

uni = mergePairs h12

unif = toJSON  uni 
unip = encodePretty unif 
unie = encode unif
