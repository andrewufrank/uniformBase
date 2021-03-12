-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Pointless
--
-- | the standard useful pointless operations
-- https://hackage.haskell.org/package/tuple could be useful
-- https://hackage.haskell.org/package/extra contains useful stuff
-----------------------------------------------------------------------------
-- {-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns          #-}
--{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
{-# OPTIONS_GHC  -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
    -- runErrorT is depreceiated but used in monads-tf
-- {-# OPTIONS_GHC -w #-}


module Uniform.Pointless
  ( module Uniform.Pointless
    -- , module Data.Tuple.Utils
  )
where

-- import Data.Tuple.Utils (fst3, snd3, thd3, fst4, snd4, trd4, fth4
--             , fst5, snd5, trd5, fth5, ffh5)


pair :: (t -> b) -> (t, t) -> (b, b)
pair f (a, b) = (f a, f b)
cross :: (t1 -> a, t2 -> b) -> (t1, t2) -> (a, b)
cross (f, g) (a, b) = (f a, g b)
swapPair :: (b, a) -> (a, b)
swapPair (a, b) = (b, a)

first :: (t -> a) -> (t, b) -> (a, b)
first f (a, b) = (f a, b)
second :: (t -> b) -> (a, t) -> (a, b)
second f (a, b) = (a, f b)

fst3 :: (a, b, c) -> a
fst3 (x, y, z) = x

snd3 :: (a, b, c) -> b
snd3 (x, y, z) = y

trd3 :: (a, b, c) -> c
trd3 (x, y, z) = z

fst4 :: (a, b, c, d) -> a
fst4 (x, y, z, w) = x

snd4 :: (a, b, c, d) -> b
snd4 (x, y, z, w) = y

trd4 :: (a, b, c, d) -> c
trd4 (x, y, z, w) = z
thd4 :: (a, b, c, d) -> c
thd4 = trd4

fth4 :: (a, b, c, d) -> d
fth4 (x, y, z, w) = w

fst5 :: (a, b, c, d, e) -> a
fst5 (x, y, z, w, u) = x

snd5 :: (a, b, c, d, e) -> b
snd5 (x, y, z, w, u) = y

thd5 :: (a, b, c, d, e) -> c
thd5 (x, y, z, w, u) = z
trd5 :: (a, b, c, d, e) -> c
trd5 = thd5

fth5 :: (a, b, c, d, e) -> d
fth5 (x, y, z, w, u) = w

ffh5 :: (a, b, c, d, e) -> e
ffh5 (x, y, z, w, u) = u


first3 :: (a1 -> b) -> (a1, a2, a3) -> (b, a2, a3)
first3 f (a1, a2, a3) = (f a1, a2, a3)

second3 :: (a2 -> b) -> (a1, a2, a3) -> (a1, b, a3)
second3 f (a1, a2, a3) = (a1, f a2, a3)

third3 :: (a3 -> b) -> (a1, a2, a3) -> (a1, a2, b)
third3 f (a1, a2, a3) = (a1, a2, f a3)

first4 :: (a1 -> b) -> (a1, a2, a3, a4) -> (b, a2, a3, a4)
first4 f (a1, a2, a3, a4) = (f a1, a2, a3, a4)

second4 :: (a2 -> b) -> (a1, a2, a3, a4) -> (a1, b, a3, a4)
second4 f (a1, a2, a3, a4) = (a1, f a2, a3, a4)

third4 :: (a3 -> b) -> (a1, a2, a3, a4) -> (a1, a2, b, a4)
third4 f (a1, a2, a3, a4) = (a1, a2, f a3, a4)

fourth4 :: (a4 -> b) -> (a1, a2, a3, a4) -> (a1, a2, a3, b)
fourth4 f (a1, a2, a3, a4) = (a1, a2, a3, f a4)

