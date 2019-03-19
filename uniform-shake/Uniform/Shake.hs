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

module Uniform.Shake
      where

import Development.Shake (Action)
        -- (getDirectoryFiles, Action
        --     , Rules, FilePattern)
-- import Uniform.FileIO (makeRelFile)

-- import qualified Path  
-- import  Path  (Path(..), File, Dir, Abs, Rel, toFilePath)
-- import qualified Path.IO
import Uniform.Error

-- instance Exception [Text] 

runErr2action :: ErrIO a -> Action a
runErr2action op = liftIO $ do
    res <- runErr  op
    case res of
        Left msg -> throw ["runErr2action", msg]
        Right a -> return a

-- throwAction :: Text -> Action () 
-- throwAction msg = liftIO . throwIO $ msg
