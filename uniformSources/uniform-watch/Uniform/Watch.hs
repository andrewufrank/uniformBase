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
{-# LANGUAGE DeriveGeneric  #-}

-- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}

-- {-# LANGUAGE PackageImports        #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
module Uniform.Watch
  ( module Uniform.Watch
  , forkIO, killThread
  ) where

import Twitch hiding (Options, log)
import GHC.Conc.Sync
import qualified Twitch

--import Control.Concurrent.Spawn
-- import Control.Concurrent
import           Control.Concurrent (forkIO, killThread)
import Uniform.FileIO
import Uniform.Strings hiding (S, (<.>), (</>))

newtype Glob = Glob Text 
    deriving (Show, Read, Eq, Generic)
-- the globs used here (possibly use the type from system-filepath-glob
unGlob (Glob a) = a

multipleWatches :: [WatchOpType] ->  ErrIO [GHC.Conc.Sync.ThreadId]
multipleWatches ws = do 
      is <- mapM mainWatch2one ws
  
      return is
  where 
    mainWatch2one :: WatchOpType -> ErrIO GHC.Conc.Sync.ThreadId
    mainWatch2one w = callIO $ forkIO (runErrorVoid $ startWatch2 w)    

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

type WatchOpType = (Path Abs Dir, (FilePath -> ErrIO ()), [Glob])

makeWatch a b c = (a,b,c)

startWatch2 :: (Show [Text], Show (Path Abs Dir))
  => WatchOpType   -> ErrIO ()
-- | start watching for a set of files (glob patterns) in one directory
-- essentially producing lines in the minimal twitch example with a single operation
startWatch2 (path1,op,globs) = do
  -- putIOwords ["startWatch2", "\n\tpath1", showT path1, "\n\textensions", showT exts]
    -- the path1 is dir to watch -- should probably be fixed to absolute
  let globs2 = map (fromString . t2s . unGlob) globs :: [Dep]
  putIOwords ["startWatch2", "\n\tpath1", showT path1, "\n\textensions", showT globs]

  callIO $ do 
    Twitch.defaultMainWithOptions
      (twichDefault4ssg
         {Twitch.root = Just . toFilePath $ path1, Twitch.log = Twitch.NoLogger})
           $ do
                let deps = map (setTwichAddModifyDelete op) globs2 :: [Dep]
                sequence_ deps
    putIOwords ["startWatch2", "end"]

      -- return ()

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

watchMain :: [WatchOpType] -> ErrIO () ->  ErrIO () 
watchMain watches  foreverOp = -- callIO $ defaultMain $ 
    bracketErrIO
        (do
            -- first
            putIOwords ["mainWatch started"]
            -- watchTID <- callIO $ forkIO (runErrorVoid $ testWatch)
            watchTIDs <- multipleWatches watches
            foreverOp -- just to make it run forever
                  
            return watchTIDs 
            )
        (\watchTIDs      -- last
        -> do
            putIOwords ["main watch  end"]
            callIO $ mapM killThread watchTIDs
            return ()
            )
        (\_         -- during
        -> do
            putIOwords ["mainWatch run"]
            -- brackets the runs of shake runs 
            putIOwords ["mainWatch run end "]
            return ()
            )
