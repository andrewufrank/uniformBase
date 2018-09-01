----{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# OPTIONS -Wall #-}
--{-# OPTIONS -fno-warn-missing-signatures #-}


module Uniform.ListForm (module Uniform.ListForm
        , module Uniform.Zero
        , module Data.Maybe
        , module Data.Either
        , module GHC.Generics

        --     Zeros (..)
        -- , Text, pair, cross
        -- ,  fst3, snd3, thd3
        -- , first, second
--            , htf_thisModulesTests
) where


import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Data.Either
import Uniform.Zero
import GHC.Generics

class (Monoid l, Zeros (LF l)) => ListForms l   where
    type LF l
    prependOne  :: (LF l) -> l   -> l
    appendOne :: l   -> (LF l) -> l
    mkOne :: (LF l) -> l
    appendTwo  :: l  -> l   -> l

    prependOne a la = appendTwo  (mkOne a) la
    appendOne la a = appendTwo la (mkOne a)
    appendTwo = error "not implemented appendTwo for ListForm (<>)"

--instance ListForms Text where   -- in strings
--    type LF Text = Char
--    combine' = T.append
--    single' = T.singleton

