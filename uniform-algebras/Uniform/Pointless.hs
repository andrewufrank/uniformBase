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
    -- runErrorT is depreceiated but used in monads-tf
-- {-# OPTIONS_GHC -w #-}


module Uniform.Pointless (
    module Uniform.Pointless
    -- , module Data.Tuple.Utils

        )  where

-- import Data.Tuple.Utils (fst3, snd3, thd3, fst4, snd4, trd4, fth4
--             , fst5, snd5, trd5, fth5, ffh5)


pair f (a,b) = (f a, f b)
cross (f,g) (a,b) = (f a, g b)
swapPair (a,b) = (b,a)

first f (a,b) = (f a, b)
second f (a,b) = (a, f b)

fst3 :: (a,b,c) -> a
fst3 (x,y,z) = x

snd3 :: (a,b,c) -> b
snd3 (x,y,z) = y

trd3 :: (a,b,c) -> c
trd3 (x,y,z) = z

fst4 :: (a,b,c,d) -> a
fst4 (x,y,z,w) = x

snd4 :: (a,b,c,d) -> b
snd4 (x,y,z,w) = y

trd4 :: (a,b,c,d) -> c
trd4 (x,y,z,w) = z

fth4 :: (a,b,c,d) -> d
fth4 (x,y,z,w) = w

fst5 :: (a,b,c,d,e) -> a
fst5 (x,y,z,w,u) = x

snd5 :: (a,b,c,d,e) -> b
snd5 (x,y,z,w,u) = y

trd5 :: (a,b,c,d,e) -> c
trd5 (x,y,z,w,u) = z

fth5 :: (a,b,c,d,e) -> d
fth5 (x,y,z,w,u) = w

ffh5 :: (a,b,c,d,e) -> e
ffh5 (x,y,z,w,u) = u


first3 :: (a1 -> b) -> (a1, a2, a3) -> (b, a2, a3)
first3 f (a1, a2, a3) = (f a1, a2, a3)

second3 :: (a2 -> b) -> (a1, a2, a3) -> (a1, b, a3)
second3 f (a1, a2, a3) = (a1, f a2, a3)

third3 :: (a3 -> b) -> (a1, a2, a3) -> (a1, a2, b)
third3 f (a1, a2, a3) = (a1, a2, f a3)

-- -- move TODO algebras
-- fst3 (a,b,c) = a
-- snd3 (a,b,c) = b
-- thd3 (a,b,c) = c
