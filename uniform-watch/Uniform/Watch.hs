-----------------------------------------------------------------------------
--
-- Module      :  Uniform.watch
--
-- | a miniaml set of
-----------------------------------------------------------------------------
-- {-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
    -- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}

-- {-# LANGUAGE PackageImports        #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
module Uniform.Watch
  ( module Uniform.Watch
  ) where

import Twitch hiding (Options, log)

import qualified Twitch

--import Control.Concurrent.Spawn
import Control.Concurrent
import Uniform.FileIO
import Uniform.Strings hiding (S, (<.>), (</>))

twichDefault4ssg =
  Twitch.Options
    { Twitch.log = NoLogger
    , logFile = Nothing
    , root = Nothing
    , recurseThroughDirectories = True
    , debounce = Debounce
    , debounceAmount = 1 -- second? NominalTimeDifference
    , pollInterval = 10 ^ (6 :: Int) -- 1 second
    , usePolling = False
    }

mainWatch2 :: (Show [Text], Show (Path b Dir))
  => (FilePath -> ErrIO ())
  -> Path b Dir
  -> [FilePath]
  -> ErrIO ()
mainWatch2 op path1 exts = do
  putIOwords ["mainWatchThemes", "path1", showT path1, "extensions", showT exts]
    -- the path1 is dir to watch -- should probably be fixed to absolute
  let exts2 = map fromString exts :: [Dep]
  callIO $
    Twitch.defaultMainWithOptions
      (twichDefault4ssg
         {Twitch.root = Just . toFilePath $ path1, Twitch.log = Twitch.NoLogger}) $ do
      let deps = map (setTwichAddModifyDelete op) exts2 :: [Dep]
      sequence deps
      return ()

setTwichAddModifyDelete :: (FilePath -> ErrIO ()) -> Dep -> Dep
setTwichAddModifyDelete op ext =
  Twitch.addModify (\filepath -> runErrorVoid $ op filepath) (ext :: Dep)
    -- do not simplify, needs lambda for Twitch 
    -- addModify :: (FilePath -> IO a) -> Dep -> Dep

runErrorRepl :: (Show a) => a -> IO ()
-- just for testing when an event is triggered
runErrorRepl a = do
  putIOwords ["runErrorRepl", "input is", showT a]
  return ()
