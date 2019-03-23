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
--    , UndecidableInstances
    , OverloadedStrings
    , TypeFamilies

    #-}

module Main     where



import Test.Framework
import {-@ HTF_TESTS @-} Uniform.Error
import Uniform.Strings

main = do
    putIOwords ["HTF errorTest.hs:\n posTest"]
    r <- htfMainWithArgs ["--quiet"] htf_importedTests
    putIOwords ["HTF end errorTest.hs:\n posTest", showT r]
    r2 <- errorTest
    putIOwords [" errorTest2\n  ", showT r2]
    return r

errorTest ::   IO Bool
errorTest = do
    r <- runErr errorTest2
    v1 <- case r of
        Left msg -> do
                putIOwords  ["errorTest returned Left :", msg]
                return False
        Right v -> return v
    return v1



--test2 :: ErrIO Bool
--test2 = do
--    test2catch False
--    return False
--
--  `catchError` \s -> return True

errorTest2 ::   ErrIO Bool
errorTest2 = do
    c1 :: Bool <- mustFailM "test throw" $ (throwError "error1")
    c2 :: Bool <- mustSucceedM "test return ok" $ (return ())
    c3 <- mustSucceedM "test catch ok" $ test2catch True
    c4 <- mustSucceedM "test catch ok" $ test2catch False
    let res = and [c1, c2, c3, c4]
    return res


test2catch :: Bool -> ErrIO ()
test2catch b =
    if b then return ()
        else throwError "test2catch message"
  `catchError` (\e -> do
        if not b then putIOwords ["ok - error thrown and caught:", e ]
            else throwError "test2catch - error caught when not thrown"
        return ()
        )


--instance (Show a) => Strings (ErrOrVal a) where
--    toString (Left msg) = msg
--    toString (Right r) = show r



