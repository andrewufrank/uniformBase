-----------------------------------------------------------------------------
--
-- Module      :  Uniform.TwitchMinimal
--
-- | the example from readme 
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}


module Uniform.TwitchMinimal where

import Twitch
-- import System.Process ( system )


mainTwitchMinimal = defaultMain $ do
   "*.md"   |> \filePath -> putStrLn ("md file changed" ++ filePath)
   "*.html" |> \_ -> putStrLn "html file changed"