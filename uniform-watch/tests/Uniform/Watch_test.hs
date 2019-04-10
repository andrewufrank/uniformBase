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
 
mainWatch :: ErrIO () ->  ErrIO () 
mainWatch watchOp1  = -- callIO $ defaultMain $ 
    bracketErrIO
        (do
            -- first
            putIOwords ["mainWatch started"]
            -- watchTID <- callIO $ forkIO (runErrorVoid $ testWatch)
            watchOp1
            -- runScotty zero zero zero -- just to make it run forever
            return () 
            )
        (\()       -- last
        -> do
            putIOwords ["main watch  end"]
            -- callIO $ killThread (watchTID)
            return ()
            )
        (\_         -- during
        -> do
            putIOwords ["mainWatch run"]
            -- brackets the runs of shake runs 
            putIOwords ["mainWatch run end "]
            return ()
            )
    
type WatchOpType = (Path Abs Dir, (FilePath -> ErrIO ()), [Glob])


watchOp :: Path Abs Dir -> (FilePath -> ErrIO ()) -> [Glob]-> ErrIO ()
watchOp path  ops globs = mainWatch2 (path, ops, globs)
                 
testWatch = watchOp 
                (makeAbsDir "/home/frank/Workspace8/uniform/uniform-watch")
                (\f -> putIOwords ["testWatch", showT f]) 
                [Glob "*.txt"]

mainWatch3 :: 
  -- (Show [Text], Show (Path b Dir))
  -- => 
  --   (FilePath -> ErrIO ())
  -- -> Path b Dir
  -- -> [FilePath]
      ErrIO ()
mainWatch3  = callIO $ do 
    defaultMain $ do 
      -- "*.txt"  |> opx
      let deps = map (setTwichAddModifyDelete opx) ["*.txt"] :: [Dep]
      sequence deps
      return ()

opx filepath = putIOwords ["mainWatch3", "touched", s2t filepath]                    
main2 :: IO ()
main2      -- just a simple bake for test
  = do
    putStrLn "main2"
    runErrorVoid
      $ do
        mainWatch mainWatch3
        putIOwords ["mainWatch ends"]     

        return ()