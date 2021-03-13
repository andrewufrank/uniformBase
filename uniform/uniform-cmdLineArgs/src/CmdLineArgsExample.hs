-----------------------------------------------------------------------------
--
-- Module      :   an example for a command line argument setup 
--                  is a Main and starts with convenience
-- for information see https://github.com/pcapriotti/optparse-applicative
-- change the getAttr function to return Text
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

module CmdLineArgsExample where



import Test.Framework ( makeTestSuite, TestSuite )

import Uniform.Strings
    ( s2t, t2s, putIOwords, showT, unlinesT, Text, Zeros(isZero) )
import Uniform.Zero ( Zeros(isZero) )
import Uniform.Error
import Uniform.StartApp
import Uniform.FileIO
    ( homeDir2,
      makeRelFileT,
      Path,
      Abs,
      Dir,
      File,
      Filenames3(addFileName) )
--  import Uniform.Convenience.StartApp ( startProg )
import           Data.Semigroup                 ( (<>) )
import Options.Applicative.Builder
  
import Options.Applicative ( Parser, execParser, helper )

programName = "CmdLineArgsExample.hs"
progTitle = "example for command line argument processing" :: Text

main :: IO ()
main = startProg
  programName
  progTitle
  (do
    inp :: Inputs <- parseArgs2input
      "makeReport.txt"  -- the default filename 
      (unlinesT
        [ "a flag, a flag (characters), a filename"
        , "all value default, nothing enforced"
        ]
      )
      "dir relative to home"
    mainExample inp
  )
-- | the command line arguments raw 
--  number of args must correspond in order and number with the 
--  command arguments described in the parser
data LitArgs = LitArgs
  { argSwitch1 :: Bool -- ^ l - a swtich
  , argFlag1 :: String -- ^ b - a string option
  , argInFile1  ::  String  -- ^ g - an otpional filename
   } deriving (Show)

cmdArgs :: Parser LitArgs
-- | strings which have no default result in enforced arguments
-- order and type of arguments must correspod to LitArgs
cmdArgs =
  LitArgs
    <$> switch
          (  long "lswitch"
          <> short 'l'
          -- <> value True  -- default False, if not set
          <> help "switch (default False) "
          )
    <*> strOption
          (  long "flag1"
          <> short 'b'
          <> value "Medium"
          <> metavar "metavar flag needed."
          <> help "flag (default Medium)"
          )
    <*> strOption
          (  long "file1 (optional)"
          <> short 'g'
          <> metavar "File1"
          <> value ""
          <> help "filename (default homeDir)"
          )

-- | the arguments in the program usable format
data Inputs = Inputs
        { inFile1 :: Path Abs File
        , flag1 :: Text
        , switch1 :: Bool
        } deriving (Show, Read, Eq)

mainExample inp = do
  putIOwords ["example for Command Line Argument processing"]

  putIOwords ["inputs on command line:", showT inp]
  return ()

parseArgs2input :: Text -> Text -> Text -> ErrIO Inputs
-- getting cmd line arguments, produces the input in the usable form
--  with a default value for the file name
-- the two text arguments are used in the cmd arg parse
-- is specific to the parser (and thus to the cmd line arguments

parseArgs2input filenameDefault t1 t2 = do
  args1 <- getArgsParsed t1 t2
  putIOwords ["parseArgs2input: args found", showT args1]

  --    let server = selectServer args :: ServerFlag
  homeDir :: Path Abs Dir <- homeDir2
  let inFile1 = s2t . argInFile1 $ args1

  let filename1 = if isZero inFile1
        then addFileName homeDir (makeRelFileT filenameDefault)
        else addFileName homeDir (makeRelFileT inFile1) :: Path Abs File

  let inputs1 = Inputs { inFile1 = filename1
                       , flag1   = s2t $ argFlag1 args1
                       , switch1 = argSwitch1 args1
                       }

  putIOwords ["parseArgs2input:  inputs ", showT inputs1]
  return inputs1


getArgsParsed :: Text -> Text -> ErrIO LitArgs
getArgsParsed t1 t2 = do
  args <- callIO $ execParser opts
  return args
 where
  opts = info (helper <*> cmdArgs)
              (fullDesc <> (progDesc . t2s $ t1) <> (header . t2s $ t2))
