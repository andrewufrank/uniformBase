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
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Uniform.PathShowCase

where

import Uniform.Strings
-- import Uniform.Error

import Path
-- import Data.Typeable
-- import Data.Data



readsPrecForPath :: ([Char] -> Maybe a)
                          -> [Char] -> String -> [Char] -> [(a, [Char])]
readsPrecForPath parseAD prefix1 msg a0 =
  if (prefix1 `isPrefixOf'` a1 )
    then  [ (res2, rem2)]
    else error ("not a  prefix for " ++ msg ++ " input " ++ show a1)

  where
    a1 = dropWhile isSpace a0
    a2 = stripPrefix' prefix1 a1
    a3  = fromJustNote "readPrec not prefix"  a2
    (a4,rem2) = span terminate a3  -- what else could be terminating?
    res1 = parseAD a4   -- there seem not to be a parser for filepath
    res2 = fromJustNote (unwords["not a path ", msg, "input", show a0]) res1
    terminate :: Char -> Bool
    terminate c = not (c `elem`   [',','}'])
    -- add here character to stop reading !!

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
    -- show a = concat' [prefixRelFile,  "\"", toFilePath a, "\""]
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

toFilePathT :: Path b t -> Text
toFilePathT = s2t . toFilePath

prefixAbsDir, prefixAbsFile, prefixRelDir, prefixRelFile :: String
prefixAbsFile = "Path Abs File "
prefixAbsDir =  "Path Abs Dir "
prefixRelFile = "Path Rel File "
prefixRelDir = "Path Rel Dir "
