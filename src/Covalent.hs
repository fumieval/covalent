{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Covalent where

import Control.Applicative
import Data.Functor.Identity
import GHC.Generics
    ( Generic(..),
      U1(..),
      K1(K1),
      M1(M1),
      type (:+:)(..),
      type (:*:)(..) )

class Covalent c a where
  traverseCommon :: Applicative f => (forall x. c x => x -> f x) -> a -> f a
  default traverseCommon :: (Generic a, Applicative f, GCovalent c (Rep a)) => (forall x. c x => x -> f x) -> a -> f a
  traverseCommon f = fmap to . gtraverseCommon @c f . from

class GCovalent c t where
  gtraverseCommon :: Applicative f => (forall x. c x => x -> f x) -> t a -> f (t a)

instance GCovalent c U1 where
  gtraverseCommon _ U1 = pure U1

instance (c a) => GCovalent c (K1 t a) where
  gtraverseCommon f (K1 a) = K1 <$> f a

instance (GCovalent c t) => GCovalent c (M1 i s t) where
  gtraverseCommon f (M1 a) = M1 <$> gtraverseCommon @c f a

instance (GCovalent c a, GCovalent c b) => GCovalent c (a :+: b) where
  gtraverseCommon f (L1 a) = L1 <$> gtraverseCommon @c f a
  gtraverseCommon f (R1 a) = R1 <$> gtraverseCommon @c f a

instance (GCovalent c a, GCovalent c b) => GCovalent c (a :*: b) where
  gtraverseCommon f (a :*: b) = (:*:) <$> gtraverseCommon @c f a <*> gtraverseCommon @c f b

viewsCommon :: forall c a b. (Covalent c a, Monoid b) => (forall x. c x => x -> b) -> a -> b
viewsCommon f = getConst . traverseCommon @c (Const . f)
{-# INLINE viewsCommon #-}

overCommon :: forall c a. Covalent c a => (forall x. c x => x -> x) -> a -> a
overCommon f = runIdentity . traverseCommon @c (Identity . f)
{-# INLINE overCommon #-}