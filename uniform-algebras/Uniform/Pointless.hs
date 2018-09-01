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
    , module Data.Tuple.Utils

        )  where

import Data.Tuple.Utils (fst3, snd3, thd3)


pair f (a,b) = (f a, f b)
cross (f,g) (a,b) = (f a, g b)
swapPair (a,b) = (b,a)

first f (a,b) = (f a, b)
second f (a,b) = (a, f b)

-- -- move TODO algebras
-- fst3 (a,b,c) = a
-- snd3 (a,b,c) = b
-- thd3 (a,b,c) = c
