-----------------------------------------------------------------------------
--
-- Module      :  Uniform.StartApp
--
-- | start an application - bridge from the ErrIO functions to
-- the standard IO monad
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


module Uniform.StartApp (module Uniform.StartApp
    -- , module Uniform.Strings
    -- , module Safe
    -- , module Control.Monad.Error  -- is monads-tf
    -- , module Control.Exception   -- to avoid control.error

        )  where
import Uniform.Error

startProg :: Show a => Text -> Text -> ErrIO a -> IO ()
startProg programName   progTitle mainProg = do  
        putIOwords [    "------------------ ", 
                        programName ,   progTitle, 
                        " ----------------------------"]
        r <- runErr $ mainProg
        putIOwords 
            [ "\n------------------", "main", progTitle
            , "\nreturning", either id showT r
            , "\n -------------------------"]
        return ()
    `catchError` (\e  -> do
            putIOwords 
                [ "startProg error caught\n", programName, progTitle
                , "\n", showT e ] -- " showT msg])
            return ()
            )

startMinimal :: Show a => Text ->  ErrIO a -> IO ()
startMinimal programName  mainProg = do  
        putIOwords  [    "------------------ " 
                    ,     programName  
                    ,    " ----------------------------\n"]
        r <- runErr $ mainProg
        putIOwords 
            [ "\n------------------", "main", programName
            , "\nreturning", either id showT r
            , "\n -------------------------"]
        return ()
    `catchError` (\e  -> do
            putIOwords 
                [ "startProg error caught\n", programName 
                , "\n", showT e ]  
            return ()
            )