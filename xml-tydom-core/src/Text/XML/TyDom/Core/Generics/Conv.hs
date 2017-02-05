{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

{-|
Module      : Text.XML.TyDom.Core.Generics.Conv
Description : Generic producer for Conv instances.
Copyright   : (c) Jonathan Merritt 2017
License     : BSD3
Maintainer  : j.s.merritt@gmail.com
Stability   : Experimental
Portability : POSIX
-}
module Text.XML.TyDom.Core.Generics.Conv (genericConv) where

import Text.XML.TyDom.Core.Types (Conv (conv))

import GHC.Generics

-- | Generic producer for a 'Conv' instance.
genericConv :: (Generic a, Generic b, GConv (Rep a) (Rep b)) => a -> b
genericConv = to . gConv . from

class GConv a b where
    gConv :: a r -> b r

instance Conv a b => GConv (K1 i a) (K1 j b) where
    gConv = K1 . conv . unK1
instance GConv a b => GConv (M1 i c a) (M1 j d b) where
    gConv = M1 . gConv . unM1
instance GConv V1 V1 where
    gConv = id
instance (GConv f1 f2, GConv g1 g2) => GConv (f1 :*: g1) (f2 :*: g2) where
    gConv (l :*: r) = gConv l :*: gConv r
