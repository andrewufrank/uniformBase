{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE OverloadedStrings
    , RecordWildCards
    , AllowAmbiguousTypes     #-}

{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- {-# OPTIONS -Wall #-}
--{-# OPTIONS -fno-warn-missing-signatures #-}

module Uniform.Shake.Path
    ( getHashedShakeVersionP
    , getDirectoryFilesP
    , replaceExtension
    , stripProperPrefix   -- import because runs in IO (' runs in ErrIO)
    , stripProperPrefixP
    , needP
    , wantP
    , ($%>)
    , ($&%>)
    -- , ($?==)  -- not relevant, needP return toFilePath in matching
    , orderOnlyP
    , cmd
    , liftIO
    , CmdOption(..)
    , (?>)
    , (?==)
    , phony
    , want
    , (</>)
    , (<.>)
    , module Uniform.FileIO
    , module Uniform.Strings
--     , module Path
    -- , module Path.IO
    , module Development.Shake.FilePath
    , module Development.Shake
    )
where

import           Development.Shake
import           Development.Shake.FilePath     ( isAbsolute
                                                , isRelative
                        -- , isFile, isDir
                                                )
        -- (getDirectoryFiles, Action
        --     , Rules, FilePattern)
import           Uniform.FileIO
import           Uniform.Strings         hiding ( (</>)
                                                , (<.>)
                                                ) -- (Text, t2s)
-- import qualified Path  
import           Path                           ( Path
                                                , File
                                                , Dir
                                                , Abs
                                                , Rel
                                                , toFilePath
                                                , stripProperPrefix
                                                )
-- get </> and <.> from here

-- import qualified Path.IO

replaceExtension :: Text -> Path a File -> Path a File
-- a flipped version of -<.> 
replaceExtension newext filep =
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

getHashedShakeVersionP :: [Path r File] -> IO String
getHashedShakeVersionP = getHashedShakeVersion . map toFilePath

class Path2nd  a c where
    stripProperPrefixP :: Path a b -> Path a c -> Path Rel c
    -- ^ strip the first (the prefix) from the second and returns remainder 
    -- throws error when not prefix or not proper file path 
instance Path2nd  a File where
    stripProperPrefixP a b = fromJustNote
        ( t2s
        . unwords'
        $ ["Path2nd Dir - not a prefix", showT a, "for", showT b]
        )
        (fmap makeRelFile ab)
        where ab = stripPrefix' (toFilePath a) (toFilePath b) :: Maybe FilePath
instance Path2nd  a Dir where
    stripProperPrefixP a b = fromJustNote
        ( t2s
        . unwords'
        $ ["Path2nd Dir - not a prefix", showT a, "for", showT b]
        )
        (fmap makeRelDir ab)
        where ab = stripPrefix' (toFilePath a) (toFilePath b) :: Maybe FilePath

needP :: [Path r File] -> Action ()
needP = need . map toFilePath

wantP :: [Path r File] -> Rules ()
wantP = want . map toFilePath

($%>) :: Path r File -> Action () -> Rules ()
p $%> a = toFilePath p %> const a

-- makeFile :: FilePath -> Path r File 
-- would need a phantom type parameter :: Path r File -> FilePath -> Path r File 
-- makeFile a = if isAbsolute a   
--                         then (makeAbsFile a :: Path Abs File)
--                         else (makeRelFile a :: Path Rel File)

($&%>) :: [Path r File] -> Action () -> Rules ()
ps $&%> a = map toFilePath ps &%> const a

-- ($?==) :: FilePattern -> Path r File -> Bool
-- pt $?== a = pt ?== (toFilePath a)
-- not usable, the need (even when using needP) gives always a filepath 

orderOnlyP :: [Path r File] -> Action ()
orderOnlyP = orderOnly . map toFilePath
