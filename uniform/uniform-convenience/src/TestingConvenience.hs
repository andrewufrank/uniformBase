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

module TestingConvenience      where


--import System.Exit
import Uniform.Zero

import Test.Framework
import Uniform.Zero
--import {-@ HTF_TESTS @-} Uniform.Zero
import Uniform.Strings


convenienceTest ::   IO Bool
convenienceTest =
    return True
    -- no tests






