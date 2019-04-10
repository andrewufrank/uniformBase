-----------------------------------------------------------------------------
--
-- Module      :  Uniform.watch_test
--
-- | a miniaml set of  
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- {-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DeriveGeneric  #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}


module Uniform.Watch_test where

import           Test.Framework
import           Uniform.Strings hiding ((</>), (<.>), (<|>))
import Uniform.Watch

import   Uniform.Error
import Uniform.Strings
import Uniform.FileIO 
import           Control.Concurrent (forkIO, killThread)
import           Uniform.Convenience.StartApp (startProg)
-- import Uniform.Watch (mainWatch2)
import Uniform.WebServer
import Control.Exception
import Twitch 
 
mainWatch :: [WatchOpType] ->  ErrIO () 
mainWatch watches  = -- callIO $ defaultMain $ 
    bracketErrIO
        (do
            -- first
            putIOwords ["mainWatch started"]
            -- watchTID <- callIO $ forkIO (runErrorVoid $ testWatch)
            watchTIDs <- multipleWatches watches
            runScotty zero zero zero -- just to make it run forever
                  -- the zeros produce strange output!
            return watchTIDs 
            )
        (\watchTIDs      -- last
        -> do
            putIOwords ["main watch  end"]
            callIO $ mapM killThread (watchTIDs)
            return ()
            )
        (\_         -- during
        -> do
            putIOwords ["mainWatch run"]
            -- brackets the runs of shake runs 
            putIOwords ["mainWatch run end "]
            return ()
            )
    

watchOp :: Path Abs Dir -> (FilePath -> ErrIO ()) -> [Glob]-> ErrIO ()
watchOp path  ops globs = mainWatch2 (path, ops, globs)
testWatch, testWatch2 :: WatchOpType                
testWatch = makeTriple 
                (makeAbsDir "/home/frank/Workspace8/uniform/uniform-watch")
                (\f -> putIOwords ["testWatch", showT f]) 
                [Glob "*.txt"]

testWatch2 = makeTriple 
  (makeAbsDir "/home/frank/Workspace8/uniform/uniform-watch")
  (\f -> putIOwords ["testWatch2", showT f]) 
  [Glob "*.html", Glob "*.md"]

makeTriple a b c = (a,b,c)

mainWatch3 ::  ErrIO ()
mainWatch3  = callIO $ do 
    defaultMain $ do 
      -- "*.txt"  |> opx
      let deps = map (setTwichAddModifyDelete opx) ["*.txt"] :: [Dep]
      sequence deps
      return ()
  where
      opx filepath = putIOwords ["mainWatch3", "touched", s2t filepath]                    
