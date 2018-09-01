--{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
 -- {-# OPTIONS -Wall #-}


module Uniform.Convenience.StartApp(
    module Uniform.Convenience.StartApp
    , module Uniform.Error
--    , module Uniform.Strings
        )   where


import           Uniform.Error
--import           Uniform.Strings


startProg :: Show a => Text -> Text -> ErrIO a -> IO ()
startProg programName   progTitle mainProg = do  -- (mainProg prefsfilename gladefilename ) = do
--        putIOwords ["the files to start with \n"
--            ,"\n", "prefsfile", prefsfilename
--            , "\ngladefile", gladefilename]
        putIOwords [ "------------------ ", programName , progTitle, " ----------------------------"]
        r <- runErr $ mainProg
        putIOwords ["main", progTitle, "\nreturning", either id showT r, "\n -------------------------"]
        return ()
    `catchError` (\e  -> do
            putIOwords ["startProg error caught", programName, progTitle, showT e ] -- " showT msg])
            return ()
            )
