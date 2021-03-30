-- {-# LANGUAGE DeriveAnyClass        #-}
-- {-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- {-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
-- {-# OPTIONS -Wall #-}
--{-# OPTIONS -fno-warn-missing-signatures #-}

module Uniform.Shake ( 
          module Uniform.Shake
        , getDirectoryToBake
        , module Uniform.Shake.Path
        , takeBaseName, splitPath 
        , Action
        , module Uniform.FileIO
        , module Uniform.Strings
    --     , module Path
        -- , module Path.IO
        -- , module Development.Shake.FilePath
        -- , module Development.Shake
        , shakeArgs, shake, ShakeOptions(..), shakeOptions
        , Verbosity(..), Lint(..)
        , need, (%>), want, phony
        )      where

import Development.Shake -- (Action, FilePattern, getDirectoryFiles)
import Development.Shake.FilePath (takeBaseName, splitPath
                        )
        -- (getDirectoryFiles, Action
        --     , Rules, FilePattern)
-- import Uniform.FileIO (makeRelFile)

-- import qualified Path  
-- import  Path hiding ((</>)) 
                -- (Path(..), File, Dir, Abs, Rel, toFilePath)
-- import qualified Path.IO
import Uniform.Error
import Uniform.Shake.Path
import Uniform.FileIO 
import Uniform.Strings -- hiding ((</>), (<.>))

($-<.>) :: Path a File -> Text ->  Path a File
f $-<.> e = replaceExtension' e f 

($--<.>) :: Path a File -> Text ->  Path a File
f $--<.> e = replaceExtension2 e f 

replaceExtension' :: Text -> Path a File -> Path a File
-- a flipped version of -<.> 
replaceExtension' newext  =
    setExtension (makeExtension . t2s $ newext) 
replaceExtension2 :: Text -> Path a File -> Path a File
-- remove a doubled extension (e.g. gutenberg.txt)
replaceExtension2 newext  =
    setExtension (makeExtension . t2s $ newext) . removeExtension

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
-- should be in Error

-- liftErrIO :: ErrIO a -> Action a
-- liftErrIO = runErr2action 

runErr2action :: ErrIO a -> Action a
runErr2action op = liftIO $ do
    res <- runErr  op
    case res of
        Left msg -> throw ["runErr2action", msg]
        Right a -> return a

-- throwAction :: Text -> Action () 
-- throwAction msg = liftIO . throwIO $ msg

getDirectoryToBake :: Text -> Path Abs Dir -> [FilePattern] 
        -> Action [Path Rel File]
-- get all files according to the FilePattern (see Shake docs)
-- but excludes all filepath which contain one of the strings in 
-- the first argument to allow directories which are not baked

getDirectoryToBake exclude d p = do
    res :: [Path Rel File] <- getDirectoryFilesP d p
    let filtered = filter (not . (isInfixOf' exclude) . toFilePathT  ) res
    -- putIOwords [unlines' $ map (s2t . toFilePath) filtered]
    return   filtered
