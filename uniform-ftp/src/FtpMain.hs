-----------------------------------------------------------------------------
--
-- Module      :   top tests  
-----------------------------------------------------------------------------
 
    {-# LANGUAGE
    MultiParamTypeClasses
    , TypeSynonymInstances
--    , FunctionalDependencies
    , FlexibleInstances
    , FlexibleContexts
    , ScopedTypeVariables
--    , UndecidableInstances
    , OverloadedStrings
    , TypeFamilies

    #-}

module Main     where

import Uniform.Strings 
import Uniform.Error 

import Uniform.Ftp 
main = do
    putIOwords ["start FTP main" ]
    push2
    putIOwords [" end FTP main"]
    return ()





