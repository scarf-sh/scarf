{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Nomia.Namespace where

import Control.Monad
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Text
import Data.UUID
import Development.Placeholders
import Nomia.Name
import Type.Reflection

newtype AnomicNameType a = AnomicNameType UUID

eqAnomicNameType :: forall a b. (Typeable a, Typeable b) => AnomicNameType a -> AnomicNameType b -> Maybe (a :~~: b)
eqAnomicNameType (AnomicNameType u1) (AnomicNameType u2) = case u1 == u2 of
  True -> case eqTypeRep (typeRep @a) (typeRep @b) of
    Just HRefl -> Just HRefl
    Nothing -> error "you constructed a bad AnomicNameType!" -- TODO is this the right way to signal "this is a programmer error" rather than "I couldn't bother to fix this"?
  False -> Nothing

data Namespace = Namespace
  { makeAnomicInNs :: forall a. Typeable a => Text -> AnomicNameType a -> Maybe a
  -- TODO How do we handle compositions? Should this be in some monad?
  -- Any way to do complex error types? Maybe shove it into the a, and Nothing means "I don't know this UUID"?
  -- Or AnomicNameType can have an e parameter...
  }

newtype Resolver = Resolver (HashMap Text (Params -> Namespace))

makeAnomic :: (Typeable a) => Resolver -> Name -> AnomicNameType a -> Maybe a
makeAnomic (Resolver nsmap) (AtomicName (PrimitiveNamespace params nsid) nm) ant = do
  -- TODO Differentiate failed lookups from failed make anomics
  ns <- Map.lookup nsid nsmap
  makeAnomicInNs (ns params) nm ant
makeAnomic (Resolver _nsmap) (AtomicName (NameNamespace _nsnm) _nm) _ant =
  $notImplemented -- Resolve to a handle, call makeAnomicInNs there
