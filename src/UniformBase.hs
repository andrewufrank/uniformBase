{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UniformBase 
    ( module UniformBase
    , module Uniform.Zero
    , module Uniform.Pointless
    , module Uniform.Error
    , module Uniform.Strings
    , module Uniform.FileIO
    , module Uniform.Json
    , module Uniform.Yaml 
    , module Uniform.Time
    -- , module Uniform.Pandoc
    -- , module Uniform.StartApp   -- in package Error 
    -- , module Uniform.CmdLineArgs 
    -- , Uniform.FileIO
    ) where

import Uniform.Zero
    ( Generic(..),
      Maybe(..),
      Either(..),
      U1(..),
      K1(..),
      M1(..),
      type (:*:)(..),
      either,
      maybe,
      fromLeft,
      fromRight,
      isLeft,
      isRight,
      lefts,
      partitionEithers,
      rights,
      catMaybes,
      fromJust,
      fromMaybe,
      isJust,
      isNothing,
      listToMaybe,
      mapMaybe,
      maybeToList,
      GZero(..),
      Zeros(..) )
import Uniform.Pointless
    ( cross,
      ffh5,
      first,
      first3,
      first4,
      fourth4,
      fst3,
      fst4,
      fst5,
      fth4,
      fth5,
      pair,
      second,
      second3,
      second4,
      snd3,
      snd4,
      snd5,
      swapPair,
      thd4,
      thd5,
      third3,
      third4,
      trd3,
      trd4,
      trd5 )
    ( cross,
      ffh5,
      first,
      first3,
      first4,
      fourth4,
      fst3,
      fst4,
      fst5,
      fth4,
      fth5,
      pair,
      second,
      second3,
      second4,
      snd3,
      snd4,
      snd5,
      swapPair,
      thd4,
      thd5,
      third3,
      third4,
      trd3,
      trd4,
      trd5 )
import Uniform.Strings 
import Uniform.FileIO
import Uniform.Error
import Uniform.Json
import Uniform.Yaml hiding (encode,decode)
    -- export qualified as Y.encode, Y.decode)
import Uniform.Time 