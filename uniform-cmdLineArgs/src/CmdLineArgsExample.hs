-----------------------------------------------------------------------------
--
-- Module      :   an example for a command line argument setup 
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

    {-# LANGUAGE
    MultiParamTypeClasses
    , TypeSynonymInstances
--    , FunctionalDependencies
    , FlexibleInstances
    , FlexibleContexts
    , ScopedTypeVariables
--    , UndecidableInstances
    , OverloadedStrings
    , TypeFamilies

    #-}

module Main     where



import Test.Framework

import Uniform.Strings

-- | the command line arguments raw
data LitArgs = LitArgs
  { argSwitch1 :: Bool -- ^ l - a swtich
  , argFlag1 :: String -- ^ b - a string
  , argInFile1  :: Maybe String  -- ^ g - an otpional filename
   } deriving (Show)

-- | the arguments in the program usable format
data Inputs = Inputs 
        { inFile1 :: Path Abs File
        , flag1 :: Text
        , switch1 :: Bool
                    } deriving (Show, Read, Eq)

main = do
    putIOwords ["example for Command Line Argument processing"]
    r <- parseArgs2input
    putIOwords ["inputs on command line:", showT r]
    return r

parseArgs2input :: Bool -> Text -> Text -> Text ->  ErrIO Inputs
-- getting cmd line arguments, with a default value for the file1
-- the two text arguments are used in the cmd arg parse
parseArgs2input debugFlag filenameDefault t1 t2  = do
    args1 <- getArgsParsed t1 t2
    putIOwords ["parseArgs2input: args found", showT args1]

    --    let server = selectServer args :: ServerFlag
    homeDir :: Path Abs Dir <- homeDir2
    let inFile1 = argFile1 $ args1 

    let filename1 = if null inFile1   
        then addFileName homeDir (makeRelFile filenameDefault)
        else addFileName homeDir (makeRelFile inFile1)
                                    :: Path Abs File

    let inputs1 = Inputs { inFile1 = filename1
                         , flag1 = argFlag1
                         , switch1 = argSwitch1} 
    
    putIOwords ["parseArgs2input:  inputs ", showT inputs1]
return inp

cmdArgs :: Parser LitArgs
cmdArgs = LitArgs
     <$> switch
          ( long "swtich1" <>
            short 'l' <>
            value True
            help "switch (default True) "
     <*> strOption
          ( long "flag1" <>
            short 'b' <>
            value "Medium" <>
            metavar "metavar flag needed." <>
            help "flag (default Medium)" )
     <*> strOption
          ( long "file1 (optional)" <>
            short 'g' <>
            metavar "File1" <>
            value "" <>
            help "filename (default homeDir)" )

getArgsParsed :: Text -> Text -> ErrIO LitArgs
getArgsParsed t1 t2 = do
        args <- callIO $ execParser opts
        return args
    where
        opts = info (helper <*> cmdArgs)
            ( fullDesc
            <> (progDesc . t2s $ t1)
            <> (header . t2s $ t2 ))
