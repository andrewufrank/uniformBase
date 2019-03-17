module Development.Shake.Path
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

import Path
import Path.IO

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
