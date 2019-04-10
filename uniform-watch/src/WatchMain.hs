-----------------------------------------------------------------------------
--
-- Module      :   top tests for layout
-----------------------------------------------------------------------------
 

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



import   Uniform.Error
import Uniform.Strings
import Uniform.FileIO 
import Uniform.Watch_test
import Uniform.Convenience.StartApp

main = startProg
    "WatchMain"
    "testing watch"
            (do
                mainWatch testWatch
            )

