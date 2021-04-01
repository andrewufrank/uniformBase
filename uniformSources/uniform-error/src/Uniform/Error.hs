-----------------------------------------------------------------------------
--
-- Module      :  Uniform.Error
--
-- | a miniaml set of error processing
-- uses monads-tf  -- family used (not fp)
-- and other monads often used (state)
-- collects from eithererror package what is working (with monads-tf)
-----------------------------------------------------------------------------
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
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


module Uniform.Error (module Uniform.Error
    , module Uniform.Strings
    , module Safe
    , module Control.Monad.Error  -- is monads-tf
    , module Control.Exception   -- to avoid control.error
        )  where

import "monads-tf" Control.Monad.Error
import Control.Exception
import Safe
import Uniform.Strings hiding ((</>), (<.>), S)

instance CharChains2 IOError Text where
    show' = s2t . show

type ErrOrVal = Either Text

type ErrIO  = ErrorT Text IO
-- an instance of Control.Monad.Error for ErrIO is automatic

instance Exception [Text] 
-- necessary to use throw in IO monad 

--catchError :: (ErrIO a) -> ErrIO a -> ErrIO a
---- | redefine catchError - the definition in monads-tf seems broken
--catchError = catch

toErrOrVal :: Either String a -> ErrOrVal a
toErrOrVal (Left s) = Left (s2t s)
toErrOrVal (Right r) = Right r

-- | runErr to avoid the depreceated message for runErrorT, which is identical
runErr :: ErrIO a -> IO (ErrOrVal a)
runErr = runErrorT

runErrorVoid :: ErrIO () -> IO ()
-- ^ run an operation in ErrIO which is not returning anything
-- simpler to use than runErr
runErrorVoid a = do
                    res <- runErr a
--                    putIOwords ["runErrorVoid", showT res]
                    case res of
                        Left msg -> error (t2s msg)
                        Right _ -> return ()
--
undef :: Text -> a
undef = error . t2s
-- ^ for type specification, not to be evaluated

fromRightEOV :: ErrOrVal a -> a
fromRightEOV (Right a) = a
fromRightEOV (Left msg) = errorT ["fromright", msg]

bracketErrIO
        ::
            ErrIO a         -- ^ computation to run first (\"acquire resource\")
        -> (a -> ErrIO b)  -- ^ computation to run last (\"release resource\")
        -> (a -> ErrIO c)  -- ^ computation to run in-between
        -> ErrIO  c          -- returns the value from the in-between computation
--bracketErrIO before after thing = bracket before after thing
-- no way to catch IO errors reliably in ErrIO -- missing Monad Mask or similar
bracketErrIO before after thing =  (fmap fromRightEOV) .  callIO $
    bracket
        (do
            ra <- runErr $ before
            return ra) --  (ra :: ErrOrVal a) )
        (\a -> runErr $ after  . fromRightEOV  $ a )
        (\a -> runErr $ thing . fromRightEOV  $ a)



instance Error Text where
-- noMsg = Left ""
-- strMsg s = Left s

callIO ::  (MonadError m, MonadIO m, ErrorType m ~ Text) => IO a -> m a
-- this is using now catch to grab all errors
callIO op = do
        r2 <- liftIO $ do
                    r <- op
                    return $ Right r
                `catch` (\e -> do
--                         putStrLn "callIO catch caught error\n"
                                return . Left $  (e::SomeException))
        case r2 of
            Left e -> do
--                        putIOwords ["\ncallIO Left branch\n", showT e, "throwError\n"]
                        throwError (showT e)
            Right v -> return v
--

throwErrorT :: [Text] -> ErrIO a
-- throw an error with a list of texts as a text
throwErrorT = throwError . unwordsT

maybe2error :: Maybe a -> ErrIO a
maybe2error Nothing  = fail "was Nothing"
maybe2error (Just a) = return a

errorT :: [Text] ->  a
-- ^ a list of texts is output with failure
errorT  = error . t2s . unwordsT
errorWords = errorT

fromJustNoteT :: [Text] -> Maybe a -> a
-- produce error with msg when Nothing, msg is list of texts
fromJustNoteT msgs a = fromJustNote (t2s . unlinesT $ msgs) a

fromRightNoteString ::   Text -> Either String b -> b
-- produce an error when assuming that a value is Right
fromRightNoteString msg (Left a) = errorT ["fromRight", showT a, msg]
fromRightNoteString _ (Right a) = a

fromRightNote  ::   Text -> Either Text b -> b
-- produce an error when assuming that a value is Right
fromRightNote msg (Left a) = errorT ["fromRight", showT a, msg]
fromRightNote _ (Right a) = a

headNoteT :: [Text] -> [a] -> a
-- get head with a list of texts
headNoteT msg s = headNote (t2s $ unwords' msg) s

startProg :: Show a => Text ->  ErrIO a -> IO ()
startProg programName  mainProg = do  
        putIOwords  [    "------------------ " 
                    ,     programName  
                    ,    " ----------------------------\n"]
        r <- runErr $ mainProg
        putIOwords 
            [ "\n------------------", "main", programName
            , "\nreturning", either id showT r
            , "\n"]
        return ()
    `catchError` (\e  -> do
            putIOwords 
                [ "startProg error caught\n", programName 
                , "\n", showT e ]  
            return ()
            )


-- | tools I thought could be useful for testing 
--  when writing tests which must fail
class (MonadError m) => Musts  m where
    mustFail:: Text -> f -> m Bool
    mustFailIO :: Text -> m () -> m Bool
    mustFailM :: Text -> m a -> m Bool
    mustSucceed :: Text -> Bool -> m Bool
    -- throws error if not True
    mustSucceedIO:: Text -> m () -> m Bool
    mustSucceedM :: Text -> m a -> m Bool
--    mustFailIOval :: Text -> m a -> m Bool
    mustReturnTrueB
        , mustReturnFalseB :: Text -> m Bool -> m Bool
    mustReturnValueMB :: Eq v => Text -> v -> m v -> m Bool
    mustReturnValueErr :: (Eq v, Show v) => Text -> v -> v -> m Bool

-- todo move to error
-- does not work in the above situation
mustError :: MonadError m => Text -> m a -> m Bool
mustError msg f = do
                        f
                        return False
               `catchError` \e -> return True


instance (MonadError m, MonadIO m, Show (ErrorType m)
            , m ~ ErrorT Text IO)
                => Musts m where
    mustFail st op = do
            let !a = op
            throwErrorT   ["should fail!", st]
            return False
        `catchError` \s -> do
--                    error "Test"  -- does work
                    putIOwords [st, "did fail - expected",   s]
                    return True


    mustFailIO st op = do
            op
            return False
        `catchError` \s ->  do
--                    error "Test"  -- does work
                    putIOwords [st, "did fail expected  ()",   s]
                    return True

    mustFailM st op = do
            op
            return False
        `catchError` \s ->  do
--                    error "Test"  -- does work
                    putIOwords [st, "did fail expected  ok",   s]
                    return True
    mustSucceed st op = do
            let !a = op
            if a then return a else throwErrorT [st, "did return False"]
        `catchError` \s -> do
--                    error "Test"  -- does work
                    putIOwords [st, "did fail - not expected",   s]
                    return False

    mustSucceedIO st op = do
            a <-  op
            return True
        `catchError` \s -> do
--                    error "Test"  -- does work
                    putIOwords [st, "did fail - not expected",   s]
                    return True

    mustSucceedM st op = do
            op
            return True
        `catchError` \s ->  do
--                    error "Test"  -- does work
                    putIOwords [st, "did fail - not expected",   s]
                    return False

    mustReturnTrueB st op = do
        t <- op
        unless t $ putIOwords [st, "error - ", st]
        return t

    mustReturnFalseB st op = do
        t <- op
        when t $ putIOwords [st, "error - ", st]
        return (not t)

    mustReturnValueMB st v op = do
        t <- op
        unless (t==v) $ putIOwords [st, "error - ", st]
        return (t==v)

    mustReturnValueErr st v op = do
        let !t = op
        unless (t==v) $ do
                    putIOwords [st, "error - ", showT t, "expected", showT v]
                    throwErrorT [st, "error - ", showT t, "expected", showT v]
        return (t==v)

