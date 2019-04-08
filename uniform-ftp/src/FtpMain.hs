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

module Main where

import           Uniform.Strings
import           Uniform.Error

import           Uniform.Ftp
import Uniform.Ftp 
import Uniform.Ftp_test
import Control.Monad.Trans.State 

main :: IO ()
main = do
    putIOwords ["start FTP main"]
    runErrorVoid $ do 
        (a,s)  <- runStateT mainStateIOd ftp0
        return ()
    putIOwords [" end FTP main"]
    return ()





