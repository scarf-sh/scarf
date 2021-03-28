{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Nomia.Namespace where

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
    Nothing -> error "You must generate a new UUID when constructing a new AnomicNameType!"
  False -> Nothing

-- TODO resource types go here in index?
-- TODO Should be able to recover the original (plain data) name here, and context
class ResolvedName rn where
  -- TODO type magic to have namespace-specific monads instead of IO

  -- TODO Should we have an e param for errors, or shove it into a?
  -- TODO Should we have a d param for arguments? In that case it's not really an AnomicNameType, so much as an AnomicNameOp... If we need it!
  makeAnomic :: forall a. Typeable a => rn -> AnomicNameType a -> IO (Maybe a)

data SomeResolvedName where
  SomeResolvedName :: (ResolvedName a) => a -> SomeResolvedName

data Namespace = Namespace
  {resolveInNs :: Text -> IO (Maybe SomeResolvedName)}

newtype Resolver = Resolver (HashMap Text (Params -> Namespace))

-- TODO Differentiate "unknown namespace" from "failed to resolve in namespace"
resolveName :: Resolver -> Name -> IO (Maybe SomeResolvedName)
resolveName (Resolver nsmap) (AtomicName (PrimitiveNamespace params nsid) nm) =
  case Map.lookup nsid nsmap of
    Just ns -> resolveInNs (ns params) nm
    Nothing -> pure Nothing
resolveName (Resolver _nsmap) (AtomicName (NameNamespace _nsnm) _nm) =
  $notImplemented -- Resolve to a handle, call resolveName there
