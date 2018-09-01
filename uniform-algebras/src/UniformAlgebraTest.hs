-----------------------------------------------------------------------------
--
-- Module      :   top tests for layout
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

    {-# LANGUAGE
    MultiParamTypeClasses
    , TypeSynonymInstances
--    , FunctionalDependencies
    , FlexibleInstances
    , FlexibleContexts
    , ScopedTypeVariables
    , UndecidableInstances
--    , OverloadedStrings
    , TypeFamilies

    #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

import Test.Framework
import {-@ HTF_TESTS @-} Uniform.Error
import Data.Strings

main = do
    putIOwords ["HTF LayoutTest.hs:\n posTest"]
    r <- htfMainWithArgs ["--quiet"] htf_importedTests
    putIOwords ["HTF end LayoutTest.hs:\n posTest"]
    return r

test_catch :: Bool -> IO Bool
test_catch b = do
    r <- runErr $ test2catch b
    case r of
        Left msg -> do
            putIOwords ["test2catch returned Left: ", msg]
            return False
        Right v -> return v

test2catch :: Bool -> ErrIO Bool
test2catch b =
    if b then return True
        else throwError "false"
  `catchError` (\e -> do
        putIOwords ["error thrown and caught:", e ]
        return True
        )





