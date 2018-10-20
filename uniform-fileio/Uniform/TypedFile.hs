--{-# OPTIONS_GHC -F -pgmF htfpp #-}
--{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- {-# OPTIONS -Wall #-}
-- {-# OPTIONS -fno-warn-missing-signatures #-}
{-# OPTIONS -w #-}

module Uniform.TypedFile (
        module Uniform.TypedFile
--        , textFile, textsFile
        -- , module Uniform.FileIO
        , EpochTime
)  where

--import Test.Framework

--import           Uniform.Error
import           Uniform.FileIOalgebra (Handle)
import           Uniform.Filenames as FN
import           Uniform.FileStrings
--import           Uniform.FileIO (EpochTime, getFileModificationTime)
import           Uniform.FileStatus
--import           Uniform.Strings hiding ((</>))
import qualified Path.IO (ensureDir)
import qualified Codec.Compression.GZip as GZip

data TypedFile5 a b = TypedFile5 { tpext5 :: Extension}

rdfGraphDebug = False

class (FileHandles a) =>
        TypedFiles5 a b where
-- | reads or writes  a structured file with the specified parsers or writer
-- the first parameter is the type of file, the second an arbitrary differentiation
-- to allow two file types with different extension and read
-- the b can be () if no differentiation is desired
    append5 f = errorT ["TypedFiles - no implementation for append5", showT f]
    read5 f = errorT ["TypedFiles - no implementation for read5", showT f]
    openHandle6 f = errorT ["TypedFiles - no implementation for openHandle6", showT f]
    writeHandle6 f = errorT ["TypedFiles - no implementation for writeHandle6", showT f]
    closeHandle6  f = errorT ["TypedFiles - no implementation for closeHandle6", showT f]


--    mkTypedFile5  :: TypedFile5 a b
    -- no argument, the extension is encapsulated in instance def
    -- replace by makeTyped
    write5 :: FN.Path Abs Dir -> Path Rel File -> TypedFile5 a b -> a -> ErrIO ()
    -- write a file, directory is created if not exist
    -- file, if exist, is replaced
    write5 fp fn tp  ct = do
        dirx <- Path.IO.ensureDir (unPath fp)
--        let fn2 = fn <.> tpext5 tp -- :: Path ar File
        write6 (fp </> fn  ) tp ct

    append5 :: Path Abs Dir -> Path Rel File -> TypedFile5 a b -> a -> ErrIO ()
    read5 :: Path Abs Dir -> Path Rel File -> TypedFile5 a b ->   ErrIO a
--    read5 fp fn tp   = do
----        let fn2 = fn <.> (tpext5 tp)
--        read6 (addFileName fp fn2) tp
-- problem with ambiguous type a in result

    write6 ::   Path Abs File -> TypedFile5 a b -> a -> ErrIO ()
    -- write a file, directory is created if not exist
    -- file, if exist, is replaced
    write6 fp  tp queryText = do
--        when rdfGraphDebug $
        putIOwords ["sparql Turtle write6", showT fp]
--        let fn2 = fp </> addExt lpX fn (tpext tp)  -- :: LegalPathname
        let fn2 = setExtension (tpext5 tp)  fp
        createDirIfMissing' (getParentDir fp)  -- add everywhere?
        when rdfGraphDebug $ putIOwords ["sparql Turtle createDIrIfMissing' "
                , showT (getParentDir fp)]
        hand <- openFile2handle fn2 WriteMode
--        when rdfGraphDebug $ putIOwords ["sparql Turtle write6", showT fn2]

        write2handle  hand   ( queryText) -- changed for Text not []

--        when rdfGraphDebug $ putIOwords ["sparql Turtle write6", showT fn2]
        closeFile2  hand
--        when rdfGraphDebug $ putIOwords ["sparql Turtle write6", showT fn2]

    openHandle6 ::  Path Abs File -> TypedFile5 a b -> ErrIO Handle
    -- | create the file and open the handle
    -- should attache ".tmp" to extension and when closing
    -- rename to correct filename - > transaction completed
    writeHandle6 ::   Handle -> TypedFile5 a b -> a -> ErrIO ()
    -- write a file, directory is created if not exist
    -- file, if exist, is replaced
--    writeHandle6zip ::   Bool -> Handle -> TypedFile5 a b -> a -> ErrIO ()
--    -- write a file, directory is created if not exist
--    -- file, if exist, is replaced
--    -- bool gives a flag to force gzip on input
    closeHandle6 :: Path Abs File ->TypedFile5 a b -> Handle -> ErrIO ()
    -- | close the handle - with transaction
    -- change filename from tmp to correct name
    append6 ::   Path Abs File -> TypedFile5 a b -> a -> ErrIO ()
    -- append to the file, with the same methods as in write6
    read6 ::   Path Abs File -> TypedFile5 a b ->   ErrIO a
    exist6 :: Path Abs File -> TypedFile5 a b ->   ErrIO Bool
    -- ^ check whether file exist
    exist6 fp tp = do
        let fn2 =  setExtension (tpext5 tp)  fp :: Path Abs File
        doesFileExist'  fn2

    modificationTime6 :: Path Abs File -> TypedFile5 a b -> ErrIO EpochTime
    modificationTime6 fp tp = do
        let fn2 =  setExtension (tpext5 tp)  fp :: Path Abs File
        t :: EpochTime <- getFileModificationTime fn2
--        st <- getFileStatus fn2
--        let t = getModificationTimeFromStatus st
        return t

    isTyped :: Path Abs File -> TypedFile5 a b -> Bool
    -- ^ check if a given file is of the right type (extenions, not mime type)
    isTyped fp tp = (getExtension fp) == typedExtension tp

    typedExtension :: TypedFile5 a b -> Extension
    -- ^ get the extension back
    typedExtension tp = tpext5 tp

    makeTyped :: Extension -> TypedFile5 a b
    -- make a typed file type, needs type specification!
    makeTyped ext = TypedFile5 {tpext5 = ext}

-- generic instance is not possible becuase
-- it is not known whether this is a file to open with filepath or path-io
--instance TypedFiles a b where
--    write5 fp fn tp  ct = do
--        dirx <- ensureDir fp
--        let fn2 = fn <.> tpext5 tp -- :: Path ar File
--        writeFile2 (fp </> fn2 ) ct
--    read5 fp fn tp   = do
--        let fn2 = fn <.> (tpext5 tp)
--        readFile2 (fp </> fn2)
--

instance TypedFiles5 [Text] () where
    -- file contains a list of lines (text)
--    mkTypedFile5  = TypedFile5 { tpext5 = Extension "txt"}
    write5 fp fn tp  ct = do
        dirx <- Path.IO.ensureDir (unPath fp)
        let fn2 = fn <.> tpext5 tp -- :: Path ar File
        writeFile2 (fp </> fn2 ) (unlines' ct)
--      writeFile2 (fp </> (fn <.> (tpext tp) )) . unlines'
    append5 fp fn tp  ct = do
        dirx <- Path.IO.ensureDir (unPath fp)
        let fn2 = fn <.> tpext5 tp -- :: Path ar File
        appendFile2 (fp </> fn2 ) (unlines' ct)
    read5 fp fn tp   = do
        let fn2 = fn <.> tpext5 tp
        fmap lines' . readFile2 $ fp </> fn2

    append6 fn tp ct = do
        let fn2 =   setExtension (tpext5 tp) $ fn
        appendFile2 fn2 (unlines' ct)
    write6 fn tp ct = do
        let fn2 =   setExtension (tpext5 tp) $ fn
        hand <- openFile2handle fn2 WriteMode
--        when rdfGraphDebug $ putIOwords ["triples write6", showT fn2]

        write2handle  hand (unlines'   ct)

--        when rdfGraphDebug $ putIOwords ["triples write6", showT fn2]
        closeFile2  hand
--        when rdfGraphDebug $ putIOwords ["triples write6", showT fn2]

    exist6 fn tp = do
        let fn2 =  setExtension (tpext5 tp) $ fn
        doesFileExist'  fn2

    read6 fn tp = do
        let fn2 =  setExtension (tpext5 tp) $ fn
        fmap lines' . readFile2 $ fn2

data GZip  -- just a type, no data

instance TypedFiles5 LazyByteString GZip where
-- ^ files with full triples
--    mkTypedFile5 = TypedFile5 {tpext5 = Extension "nt.gz"}

--    typedExtension tp = tpext5 tp
--    isTyped :: Path Abs File -> TypedFile5 a b -> Bool
--    isTyped fp tp = (getExtension fp) == typedExtension tp
    append6 fp  tp jsonld = do

        when rdfGraphDebug $ putIOwords ["triples append6", showT fp]
        let fn2 = setExtension (tpext5 tp)  fp

        appendFile2 fn2 (GZip.compress jsonld)


    openHandle6 fp  tp = do
        when rdfGraphDebug $ putIOwords ["openHandle6 jsonld"]
        let ext = unExtension (tpext5 tp)
        let tmpext = Extension (ext <.> "tmp")
        let fn2 = setExtension tmpext  fp
        when rdfGraphDebug $ putIOwords ["openHandle6 jsonld", showT fn2]

        hand <- openFile2handle fn2 WriteMode
        -- should create or truncate the file, but not when the dir not exist
        --https://hackage.haskell.org/package/base-4.10.0.0/docs/System-IO.html#g:5
        when rdfGraphDebug $ putIOwords ["openHandle6 jsonld", showT fn2]
        return hand

    closeHandle6  fp tp hand = do
--        when rdfGraphDebug $
        when rdfGraphDebug $ putIOwords ["closeHandle6 jsonld"]
        let ext = unExtension (tpext5 tp)
        let tmpext = Extension (ext <.> "tmp")
        closeFile2 hand
        let fn2 = setExtension tmpext  fp
        let fn1 = setExtension (tpext5 tp) fp
        renameOneFile fn2 fn1
--        when rdfGraphDebug $
        when rdfGraphDebug $ putIOwords ["closeHandle6 jsonld", showT fn2]
        return ()


    writeHandle6 hand  tp jsonld = do
--        when rdfGraphDebug $
--        putIOwords ["writeHandle6 jsonld gz"]
        r <- write2handle  hand (GZip.compress jsonld)
--        when rdfGraphDebug $
--        putIOwords ["writeHandle6 gz jsonld done ",  showT r ]
        return r

--    exist6 fp tp = do
--        let fn2 =  setExtension (tpext5 tp)  fp :: Path Abs File
--        doesFileExist'  fn2

    read6 fp  tp = error "read for jsonld is not easy and not required"
