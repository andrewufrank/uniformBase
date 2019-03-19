-----------------------------------------------------------------------------
--
-- Module      :   top tests for layout
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

import            Test.Framework
import           Uniform.Shake_test

import           Uniform.Strings

--import TestingFileIO

test_fileio = assertBool False

main :: IO ()
main = do

    putIOwords ["shake main"
    return ()
