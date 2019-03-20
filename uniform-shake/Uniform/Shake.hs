{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- {-# OPTIONS -Wall #-}
--{-# OPTIONS -fno-warn-missing-signatures #-}

module Uniform.Shake (runErr2action     
        , module Uniform.Shake.Path
        , takeBaseName, splitPath 
        , Action)
      where

import Development.Shake (Action)
import Development.Shake.FilePath (takeBaseName, splitPath
                        )
        -- (getDirectoryFiles, Action
        --     , Rules, FilePattern)
-- import Uniform.FileIO (makeRelFile)

-- import qualified Path  
-- import  Path  (Path(..), File, Dir, Abs, Rel, toFilePath)
-- import qualified Path.IO
import Uniform.Error
import Uniform.Shake.Path

-- instance Exception [Text] 

runErr2action :: ErrIO a -> Action a
runErr2action op = liftIO $ do
    res <- runErr  op
    case res of
        Left msg -> throw ["runErr2action", msg]
        Right a -> return a

-- throwAction :: Text -> Action () 
-- throwAction msg = liftIO . throwIO $ msg
