---------------------------------------------------------------------
---Module      :   Zeros
-- a naive and simple data type 
-- usable to stand in for most kinds of nothing 
--
-- includes commented out code and hints to
-- experiment with automatic generation of instances
----------------------------------------------------------------------

{-# LANGUAGE
    FlexibleInstances
--    , Safe
    , MultiParamTypeClasses
    , TypeSynonymInstances
    , FlexibleContexts
--        , UndecidableInstances
        , OverloadedStrings
--        , DeriveDataTypeable
    , DefaultSignatures
    , TypeOperators
 #-}
{-# OPTIONS -Wall #-}
--{-# OPTIONS -fno-warn-missing-signatures #-}

module Uniform.Zero
  ( module Uniform.Zero,
    module Data.Maybe,
    module Data.Either,
    module GHC.Generics,
  )
where

import Data.Either
  ( Either (..),
    either,
    fromLeft,
    fromRight,
    isLeft,
    isRight,
    lefts,
    partitionEithers,
    rights,
  )
import Data.Maybe
  ( Maybe (..),
    catMaybes,
    fromJust,
    fromMaybe,
    isJust,
    isNothing,
    listToMaybe,
    mapMaybe,
    maybe,
    maybeToList,
  )
import GHC.Generics
  ( Generic (..),
    --   Generic1(..),
    --   Datatype(..),
    --   Constructor(..),
    --   Selector(..),
    --   V1,

    --   Par1(..),
    --   Rec1(..),
    K1 (..),
    M1 (..),
    U1 (..),
    --   type (:+:)(..),
    type (:*:) (..),
    --   type (:.:)(..),
    --   R,
    --   D,
    --   C,
    --   Rec0,
    --   D1,
    --   C1,
    --   S1,
    --   URec(..),
    --   UAddr,
    --   UChar,
    --   UDouble,
    --   UFloat,
    --   UInt,
    --   UWord,
    --   prec,
    --   Associativity(..),
    --   DecidedStrictness(..),
    --   Fixity(..),
    --   FixityI(..),
    --   Meta(..),
    --   SourceStrictness(..),
    --   SourceUnpackedness(..)
  )

-- | a minimal algebraic type with nothing than an identity
--  useful to identify a specific value in a type
class Zeros z where
  zero :: z
  default zero :: (Generic z, GZero (Rep z)) => z
  zero = to gzero

  isZero, notZero :: Eq z => z -> Bool
  isZero z = zero == z
  notZero = not . isZero

--    zero = error   ("Zeros for this type not instantiated ")
--                ++ typeinfo)
--            where typeinfo = show $ typeOf zero
-- gives a difficult to track error, because the type is not known

class GZero a where
  gzero :: a x

instance GZero U1 where
  gzero = U1

instance Zeros a => GZero (K1 i a) where
  gzero = K1 zero

instance (GZero a, GZero b) => GZero (a :*: b) where
  gzero = gzero :*: gzero

instance GZero a => GZero (M1 i c a) where
  gzero = M1 gzero

instance Zeros Char where zero = ' '

--instance (Zeros a, Eq a) => Zeros [a] where zero = []
instance Zeros () where zero = ()

instance Zeros Bool where zero = False

-- contrary to the Unix tradition?

--instance Zeros String where zero = ""
--instance Zeros Strings where zero = []
instance Zeros Int where zero = 0

instance Zeros [a] where zero = []

instance (Zeros a, Zeros b) => Zeros (a, b) where zero = (zero, zero)

instance (Zeros a, Zeros b, Zeros c) => Zeros (a, b, c) where
  zero = (zero, zero, zero)

instance (Zeros a, Zeros b, Zeros c, Zeros d) => Zeros (a, b, c, d) where
  zero = (zero, zero, zero, zero)

instance
  (Zeros a, Zeros b, Zeros c, Zeros d, Zeros e) =>
  Zeros (a, b, c, d, e)
  where
  zero = (zero, zero, zero, zero, zero)

--instance Zeros Text where zero = (""::Text)
instance Zeros (Maybe a) where zero = Nothing

--instance (Eq (g f), Zeros f, Applicative g) => Zeros (g f)
--    where zero = pure zero

--instance (Eq x) => Zeros x where
--    zero = (undefined $ "Zeros for this type not instantiated")
-- unsafe overlap -- when defined in other modules
