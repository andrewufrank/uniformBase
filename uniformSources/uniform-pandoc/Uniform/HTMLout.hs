------------------------------------------------------------------------
--
-- Module      :  Uniform.HTMLout
-----------------------------------------------------------------------
-- {-# LANGUAGE BangPatterns                   #-}
{-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans 
            -fno-warn-missing-signatures
            -fno-warn-missing-methods 
            -fno-warn-duplicate-exports 
            -fno-warn-unused-imports 
            #-}
            
module Uniform.HTMLout
  ( module Uniform.HTMLout
    , extHTML
    , writeHtml5String
                        , writerExtensions
                        , writerHighlightStyle
                        , WriterOptions
                        , def
--   , Pandoc(..)
--   , module Uniform.Error   -- or at least ErrIO
--   , write8
--   , TypedFile5
--   , TypedFiles5
--   , TypedFiles7
--   , read8
--   , module Uniform.Json
  )
where
import           Uniform.Json
-- import Uniform.Pandoc
-- import Uniform.DocValue
import Uniform.FileIO 
import Uniform.PandocImports 
 
import           Text.Pandoc                    ( Pandoc(..)
                        , writeHtml5String
                        , writerExtensions
                        , writerHighlightStyle
                        , WriterOptions
                        , def
                        )
import           Text.Pandoc.Highlighting       ( tango )
                                                  
import Text.DocTemplates as DocTemplates  ( -- applyTemplate, 
        Doc(..), renderTemplate, compileTemplate, Template)
import Text.DocLayout (render)

-- | Reasonable options for rendering to HTML
html5Options :: WriterOptions
html5Options = def { writerHighlightStyle = Just tango
                   , writerExtensions     = writerExtensions def
                   }

-- writeHtml5String2 :: Pandoc -> ErrIO HTMLout
-- writeHtml5String2 pandocRes = do
--     p <- unPandocM $ writeHtml5String html5Options pandocRes
--     return . HTMLout $ p

-- type Dtemplate = Template Text

applyTemplate3 :: Path Abs File -> Value -> ErrIO HTMLout
-- needed for old ssg lts-13.12 - also changed for 15.13

-- | apply the template in the file to the text
-- for help look in ssg master.ptpl as an example
-- the description are in doctemplates (on hackage)
applyTemplate3 templName val = do
    t1 ::   Text  <- readFile2 templName 
    putIOwords ["test_readTempl", take' 300 . showT $ t1]
    -- let t2 = read (t2s t1) :: Template Text
    -- putIOwords ["test_readTempl Dtemplate", take' 300 . showT $ t2] 
    temp1     <- liftIO $ DocTemplates.compileTemplate mempty t1
    -- err1 :: Either String (Doc Text) <- liftIO $ DocTemplates.applyTemplate mempty (unwrap7 templText) (unDocValue val) 
    let tmp3 = case temp1 of
                Left msg -> error msg 
                Right tmp2 -> tmp2
    when False $ putIOwords ["applyTemplate3 temp2", take' 300 $ showT tmp3 ]
-- renderTemplate :: (TemplateTarget a, ToContext a b) => Template a -> b -> Doc a     
    let res = renderTemplate tmp3 val
    when False $ putIOwords ["applyTemplate3 res", take' 300 $ showT res ]
    let res2 =  render Nothing res
    when True $ putIOwords ["applyTemplate3 done res2", take' 300 $ showT res2 ]

    let res3 = HTMLout res2 
    return (res3 :: HTMLout) 

newtype HTMLout = HTMLout {contentHtml::Text}
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON HTMLout

 

-- a wrapper around html ready to publish
unHTMLout (HTMLout a) = a

htmloutFileType = TypedFile5 { tpext5 = extHTML } :: TypedFile5 Text HTMLout

instance Zeros HTMLout where
  zero = HTMLout zero

instance TypedFiles7 Text HTMLout where
  wrap7 = HTMLout
  unwrap7 (HTMLout a) = a

extHTML :: Extension
extHTML = Extension "html"

-------------------- fileType --- Dtemplate---------

    -- scheint nicht zu funktionieren fuer den type mit parameter? 
-- type Dtemplate = Template Text

-- extDtmpl= Extension "dtpl"

-- dtmplFileType =
--   TypedFile5 { tpext5 = extDtmpl } :: TypedFile5 Text Dtemplate

-- -- data Panrep = Panrep {panyam :: Value, panpan :: Pandoc }
-- --     deriving (Eq, Show, Read )
-- -- instance Zeros Panrep where zero = Panrep zero zero 

-- instance TypedFiles7 Text Dtemplate  where
--   -- handling Pandoc and read them into PandocText
--   wrap7 =  read . t2s  -- readNote "wrap7 for dtemplate 223d" .t2s
--   unwrap7   = showT -- id -- showT
