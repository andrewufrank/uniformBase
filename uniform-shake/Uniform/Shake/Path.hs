module Uniform.Shake.Path
    ( getHashedShakeVersionP
    , needP
    , wantP
    , ($%>)
    , ($&%>)
    , orderOnlyP
    , cmd
    , liftIO
    , CmdOption(..)
    , module Path
    , module Path.IO
    ) where

import Development.Shake
import Uniform.FileIO (makeRelFile)

import Path  
import Path.IO

getDirectoryFilesP :: Path Abs Dir -> [FilePattern] -> Action [Path Rel File] 
getDirectoryFilesP d p = do 
            res <- getDirectoryFiles (toFilePath d)
            return $ map makeRelFile res 

getHashedShakeVersionP :: [Path r File] -> IO String
getHashedShakeVersionP = getHashedShakeVersion . map toFilePath

needP :: [Path r File] -> Action ()
needP = need . map toFilePath

wantP :: [Path r File] -> Rules ()
wantP = want . map toFilePath

($%>) :: Path r File -> Action () -> Rules ()
p $%> a = toFilePath p %> const a

($&%>) :: [Path r File] -> Action () -> Rules ()
ps $&%> a = map toFilePath ps &%> const a

orderOnlyP :: [Path r File] -> Action ()
orderOnlyP = orderOnly . map toFilePath
