-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Error
--
-- | a miniaml set of error processing
-- uses monads-tf  -- family used (not fp)
-- and other monads often used (state)
-- collects from eithererror package what is working (with monads-tf)
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE BangPatterns          #-}
--{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is depreceiated but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.Error_test where

--import           "monads-tf" Control.Monad.Error
--import           Safe
import Test.Framework
    ( assertBool_,
      assertEqual_,
      makeLoc,
      makeTestSuite,
      makeUnitTest,
      TestSuite )
--import           Uniform.Strings hiding ((</>), (<.>), (<|>))
import Uniform.Error
    ( catch,
      SomeException,
      MonadError(catchError, throwError),
      putIOwords,
      bracketErrIO,
      callIO,
      runErr,
      ErrIO,
      ErrOrVal,
      Musts(mustSucceedM, mustFailM) )

import Control.Exception ( catch, SomeException )

op1 :: ErrIO ()
op1 = putIOwords ["acquire"] :: ErrIO ()
op2 :: p -> ErrIO ()
op2 h = putIOwords ["operate"]:: ErrIO ()
op3 :: p -> ErrIO ()
op3 h = putIOwords ["close"] :: ErrIO ()

test_bracket0 :: IO ()
test_bracket0 = do
    runErr $ do
                op1
                op2 ()
                op3  ()
    assertEqual True True

test_bracket1 :: IO ()
test_bracket1 = do
    runErr $ bracketErrIO op1 op3 op2
    assertEqual True True


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
        if not b then
            do
                putIOwords ["ok - error thrown and caught:", e ]
                return True
            else do
                putIOwords ["error caught when thrown"]
                throwError "test2catch - error caught when not thrown"
        return ()
        )

test_error2 :: IO ()
test_error2 = do
    r <- (runErr errorTest2)
    assertEqual (Right True :: ErrOrVal Bool)  r

test_catch2 :: IO ()
test_catch2 = do
    r <- (runErr $ test2catch True)
    assertEqual (Right () :: ErrOrVal ())  r

test_catch2f :: IO ()
test_catch2f = do
    r <- (runErr $ test2catch False)
    assertEqual (Right () :: ErrOrVal ())  r

--instance (Show a) => Strings (ErrOrVal a) where
--    toString (Left msg) = msg
--    toString (Right r) = show r


test_catch :: IO ()
test_catch =
            error "some intentional error"
       `catch` \(e::SomeException) -> assertBool True

--test_catch_error =
--            error "some intentional error"
--       `catchT` \(e :: S) -> assertBool True


test_callIO :: IO ()
test_callIO = do
        r <- runErr $ callIO $ readFile "xxxabc"
        case r of
            Left _ -> assertBool True
            Right _ -> assertBool False

--test_catch2 =
--            readFile2 "xxxabc"
--       `catch` \(e::SomeException) -> assertBool True
--
--test_catch_error2 =
--            readFile2 "xxxabcd"
--       `catchError ` \(e) -> assertBool True

--
--data SomethingBad   = SomethingBad [Text]
--    deriving D.Typeable
--unSMB (SomethingBad ss) = ss
--
--instance Show SomethingBad where
--    show (SomethingBad s) = "something bad happened:" ++ unwords s
--instance N.Exception SomethingBad
--
--throwErrorWords2 :: (N.MonadCatch m) => [Text] -> m a
--throwErrorWords2 s = N.throwM (SomethingBad s)
--
---- {-# DEPRECATED  throwErrorWords   "replace with class  throwErrorWords2 " #-}
---- the constraint MonadCatch is not possible for em
--
--throwErrorWords :: (MonadError  m, ErrorType m ~ Text)  => [Text] -> m a
--throwErrorWords s = throwError .unwords $ s   -- use only this!!
--

