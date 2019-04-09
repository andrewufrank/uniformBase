-----------------------------------------------------------------------------
--
-- Module      :   top tests for layout
-----------------------------------------------------------------------------
-- {-# OPTIONS_GHC -F -pgmF htfpp #-}

    {-# LANGUAGE
            MultiParamTypeClasses
    -- , TypeSynonymInstances
--    , FunctionalDependencies
    -- , FlexibleInstances
    -- , FlexibleContexts
    -- , ScopedTypeVariables
--    , UndecidableInstances
        , OverloadedStrings
        -- , TypeFamilies

    #-}

module Main     where



-- import Test.Framework
-- import {-@ HTF_TESTS @-} Uniform.Error
import   Uniform.Error
import Uniform.Strings
import Uniform.FileIO 
import           Control.Concurrent (forkIO, killThread)
import           Uniform.Convenience.StartApp (startProg)
import Uniform.Watch (mainWatch2)
import Uniform.WebServer
import Uniform.Watch_test

main = startProg
    "WatchMain"
    "testing watch"
            (do
                mainWatch 
            )

