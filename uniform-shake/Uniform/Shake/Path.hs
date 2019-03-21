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
    where



import           Development.Shake
-- import           Development.Shake.FilePath     ( isAbsolute
--                                                 , isRelative
--                         -- , isFile, isDir
--                                                 )
        -- (getDirectoryFiles, Action
        --     , Rules, FilePattern)
-- import           Uniform.FileIO
-- import           Uniform.Strings         hiding ( (</>)
--                                                 , (<.>)
--                                                 ) -- (Text, t2s)
-- import qualified Path  
import           Path                           ( Path
                                                , File
                                                , Dir
                                                , Abs
                                                , Rel
                                                , toFilePath
                                                -- , stripProperPrefix
                                                )


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
