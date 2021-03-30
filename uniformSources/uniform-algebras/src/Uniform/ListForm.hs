----{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- {-# LANGUAGE DeriveAnyClass        #-}
-- {-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS -Wall #-}
--{-# OPTIONS -fno-warn-missing-signatures #-}


module Uniform.ListForm (module Uniform.ListForm
        , module Uniform.Zero
        , module Data.Maybe
        , module Data.Either
        , module GHC.Generics
) where


-- import qualified Data.Text as T
import Data.Maybe
import Data.Either
import Uniform.Zero
import GHC.Generics

class (Monoid l, Zeros (LF l)) => ListForms l   where
    type LF l
    prependOne  :: LF l -> l   -> l
    appendOne :: l   -> LF l -> l
    mkOne :: LF l -> l
    appendTwo  :: l  -> l   -> l

    prependOne a la = appendTwo  (mkOne a) la
    appendOne la a = appendTwo la (mkOne a)
    appendTwo = error "not implemented appendTwo for ListForm (<>)"
