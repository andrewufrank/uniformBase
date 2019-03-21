{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- {-# OPTIONS -Wall #-}
--{-# OPTIONS -fno-warn-missing-signatures #-}

module Uniform.Shake (runErr2action, liftErrIO  
        , module Uniform.Shake
        , module Uniform.Shake.Path
        , takeBaseName, splitPath 
        , Action
        , module Uniform.FileIO
        , module Uniform.Strings
    --     , module Path
        -- , module Path.IO
        , module Development.Shake.FilePath
        , module Development.Shake
        , shakeArgs
        )      where

import Development.Shake -- (Action, FilePattern, getDirectoryFiles)
import Development.Shake.FilePath (takeBaseName, splitPath
                        )
        -- (getDirectoryFiles, Action
        --     , Rules, FilePattern)
-- import Uniform.FileIO (makeRelFile)

-- import qualified Path  
import  Path hiding ((</>)) 
                -- (Path(..), File, Dir, Abs, Rel, toFilePath)
-- import qualified Path.IO
import Uniform.Error
import Uniform.Shake.Path
import Uniform.FileIO 
import Uniform.Strings hiding ((</>), (<.>))

($-<.>) :: Text -> Path a File -> Path a File
($-<.>) = replaceExtension'

replaceExtension' :: Text -> Path a File -> Path a File
-- a flipped version of -<.> 
replaceExtension' newext filep =
    setExtension (makeExtension . t2s $ newext) filep
    -- if isRelative filen 
    --     then makeRelFile resn 
    --     else makeAbsFile resn
    --     where 
    --             filen = toFilePath filep 
    --             resn = replaceExtension (t2s newext) filen 

getDirectoryFilesP :: Path Abs Dir -> [FilePattern] -> Action [Path Rel File]
getDirectoryFilesP d p = do
    res :: [FilePath] <- getDirectoryFiles (toFilePath d) p
    return $ map makeRelFile res

copyFileChangedP :: Path Abs File -> Path Abs File -> Action ()
copyFileChangedP infile outf = copyFileChanged (toFilePath infile) (toFilePath outf)

class Path2nd  a c where
    stripProperPrefixP :: Path a b -> Path a c -> Path Rel c
    makeRelativeP  :: Path a Dir -> Path a c -> Path Rel c
    makeRelativeP = stripProperPrefixP
    -- ^ strip the first (the prefix) from the second and returns remainder 
    -- throws error when not prefix or not proper file path 
    replaceDirectoryP :: Path a Dir -> Path a Dir -> Path a c  -> Path a c
    -- ^ strip the first (the prefix) and add the second to the third 
    
instance   Path2nd  a File where
    stripProperPrefixP a b = fromJustNote
        ( t2s
        . unwords'
        $ ["Path2nd Dir - not a prefix", s2t . toFilePath $  a, "for",  s2t . toFilePath $ b]
        )
        (fmap makeRelFile ab)
        where ab = stripPrefix' (toFilePath a) (toFilePath b) :: Maybe FilePath

    replaceDirectoryP pref newpref old = newpref </> rem1 
        where rem1 = stripProperPrefixP pref old


instance Path2nd  a Dir where
    stripProperPrefixP a b = fromJustNote
        ( t2s
        . unwords'
        $ ["Path2nd Dir - not a prefix",  s2t . toFilePath $ a, "for",  s2t . toFilePath $ b]
        )
        (fmap makeRelDir ab)
        where ab = stripPrefix' (toFilePath a) (toFilePath b) :: Maybe FilePath

    replaceDirectoryP pref newpref old = newpref </> rem1 
        where rem1 = stripProperPrefixP pref old
-- instance Exception [Text] 

liftErrIO :: ErrIO a -> Action a
liftErrIO = runErr2action 

runErr2action :: ErrIO a -> Action a
runErr2action op = liftIO $ do
    res <- runErr  op
    case res of
        Left msg -> throw ["runErr2action", msg]
        Right a -> return a

-- throwAction :: Text -> Action () 
-- throwAction msg = liftIO . throwIO $ msg
