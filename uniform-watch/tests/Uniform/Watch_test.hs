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
-- {-# LANGUAGE UndecidableInstances  #-}
-- {-# LANGUAGE DeriveGeneric  #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}


module Uniform.Watch_test where

import           Test.Framework
import           Uniform.Strings  
import Uniform.Watch

import   Uniform.Error
import Uniform.Strings
import Uniform.FileIO 
import           Control.Concurrent (forkIO, killThread)
import           Uniform.Convenience.StartApp (startProg)
-- import Uniform.Watch (startWatch2)
import Uniform.WebServer
import Control.Exception
import Twitch 
 
          

foreverScotty :: ErrIO () 
foreverScotty = runScotty 3000 
    (makeAbsDir "/home/frank/Workspace8/uniform/uniform-watch")  
    (     makeRelFile "test1.html")  
-- the zeros produce strange output!


watchOp :: Path Abs Dir -> (FilePath -> ErrIO ()) -> [Glob]-> ErrIO ()
watchOp path  ops globs = startWatch2 (path, ops, globs)
testWatch, testWatch2 :: WatchOpType                
testWatch = makeWatch  
                (makeAbsDir "/home/frank/Workspace8/uniform/uniform-watch")
                (\f -> putIOwords ["testWatch", showT f]) 
                [Glob "*.txt"]

testWatch2 = makeWatch  
  (makeAbsDir "/home/frank/Workspace8/uniform/uniform-watch")
  (\f -> putIOwords ["testWatch2", showT f]) 
  [Glob "*.html", Glob "**/*.md"]

-- makeWatch a b c = (a,b,c)

mainWatch3 ::  ErrIO ()
mainWatch3  = callIO $ 
    defaultMain $ do 
      -- "*.txt"  |> opx
      let deps = map (setTwichAddModifyDelete opx) ["*.txt"] :: [Dep]
      sequence_ deps
      -- return ()
  where
      opx filepath = putIOwords ["mainWatch3", "touched", s2t filepath]                    
