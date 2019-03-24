-----------------------------------------------------------------------------
--
-- Module      :  Uniform.watch
--
-- | a miniaml set of
-----------------------------------------------------------------------------
-- {-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- {-# OPTIONS_GHC  -fno-warn-warnings-deprecations #-}
    -- runErrorT is  but used in monads-tf
{-# OPTIONS_GHC -w #-}


module Uniform.Watch (module Uniform.Watch
        )  where

import Twitch   hiding ( Options, log)
        
import qualified Twitch
--import Control.Concurrent.Spawn
import           Control.Concurrent
import           Uniform.Strings hiding ((</>), (<.>), S)
import Uniform.FileIO 


-- example for use - 
-- not likely worth to generalize
-- see SSG - serversg

-- mainWatch :: SiteLayout -> Port -> Path Abs Dir -> ErrIO ()
-- mainWatch layout bakedPort bakedPath = bracketErrIO
--     (do  -- first
--         shake layout ""
--         watchDoughTID     <- callIO $ forkIO (runErrorVoid $ watchDough layout)
--         watchTemplatesTID <- callIO $ forkIO (runErrorVoid $ watchThemes layout )
--         callIO $ scotty bakedPort (site bakedPath)
--         return (watchDoughTID, watchTemplatesTID)
--     )
--     (\(watchDoughTID, watchTemplatesTID) -> do -- last
--         putIOwords ["main2 end"]
--         callIO $ killThread (watchDoughTID)
--         callIO $ killThread (watchTemplatesTID)
--         return ()
--     )
--     (\watch -> do   -- during
--         putIOwords ["main2 run"]
-- --                        mainWatch
--         -- could here the watch for bake be included ?
--         putIOwords ["main2 run end "]
--         return ()
--     )

    
twichDefault4ssg = Twitch.Options { Twitch.log                = NoLogger
                                  , logFile                   = Nothing
                                  , root                      = Nothing
                                  , recurseThroughDirectories = True
                                  , debounce                  = Debounce
                                  , debounceAmount            = 1  -- second? NominalTimeDifference
                                  , pollInterval              = 10 ^ (6 :: Int) -- 1 second
                                  , usePolling                = False
                                  }
mainWatch2 :: (Show [Text], Show (Path b Dir)) => ( FilePath -> ErrIO ()) 
                         -> Path b Dir -> [FilePath] -> ErrIO ()
mainWatch2  op path1 exts = do
    -- let path1P = (themeDir layout) </> templatesDirName :: Path Abs Dir
    -- let bakedPath     = bakedDir layout
    putIOwords ["mainWatchThemes", "path1", showT path1, "extensions", showT exts]
    -- copy the static files, not done by shake yet
    -- runErrorVoid $ copyDirRecursive path1  path2
    -- putIOwords [programName, "copied templates all files"]
    let exts2 = map fromString exts :: [Dep]
    callIO $ Twitch.defaultMainWithOptions
        (twichDefault4ssg { Twitch.root = Just . toFilePath $ path1
                        , Twitch.log  = Twitch.NoLogger
                        }
        )
        $ do 
            -- let deps  = map  (setTwichAddModifyDelete op arg1)   exts :: [Dep]
            -- sequence  deps 
            let deps  = map  (setTwichAddModifyDelete op)   exts2 :: [Dep]  
            sequence  deps 
            return () 
            --            verbosity from Cabal
            -- Twitch.addModify
            --     (\filepath -> runErrorVoid $ shake layout filepath)
            --     "**/*.yaml"
            -- Twitch.addModify
            --     (\filepath -> runErrorVoid $ shake layout filepath)
            --     "**/*.dtpl"
            -- Twitch.addModify
            --     (\filepath -> runErrorVoid $ shake layout filepath)
            --     "**/*.css"
            -- Twitch.addModify
            --     (\filepath -> runErrorVoid $ shake layout filepath)
            --     "**/*.jpg"
            -- -- add and modify event
            --     --  "*.html" |> \_ -> system $ "osascript refreshSafari.AppleScript"


setTwichAddModifyDelete :: (FilePath -> ErrIO ()) -> Dep -> Dep
setTwichAddModifyDelete op   ext =   
    Twitch.addModify   (\filepath -> runErrorVoid $ op  filepath)(ext :: Dep)
    -- do not simplify, needs lambda for Twitch 
    -- addModify :: (FilePath -> IO a) -> Dep -> Dep

runErrorRepl :: (Show a) => a -> IO ()
-- just for testing when an event is triggered
runErrorRepl a = do
    putIOwords ["runErrorRepl", "input is", showT a]
    return ()

