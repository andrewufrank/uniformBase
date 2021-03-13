-----------------------------------------------------------------------------
--
-- Module      :   the main for shake 
-----------------------------------------------------------------------------

{-# LANGUAGE
MultiParamTypeClasses
, TypeSynonymInstances
--    , FunctionalDependencies
, FlexibleInstances
, FlexibleContexts
, ScopedTypeVariables
, UndecidableInstances
, OverloadedStrings
, TypeFamilies

#-}

module Main where  -- UniformShakeMain


import           Uniform.Strings
import Uniform.Shake 

main :: IO()
main = do
    putIOwords ["runs TestingShake.hs"]
    return ()

a = "testa" :: String
b = "test BBBB" :: Text 
