{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
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

-- TODO This will change once we have composition... Need to know which part reduces maybe?
-- TODO This will change when we have context... Pass along handles to context maybe? Or maybe a bundle of same that is opaque to the caller but can be unwrapped by the relevant ns?
data ReductionMessage = ReductionMessage
  { rm_ns :: Namespace, -- TODO Should this be optional? Probably passing on the namespace is a privilege leak in the capability world. Or maybe we could use delegation somehow...
    rm_nsid :: NamespaceId, -- This is the ID known by the source of the reduction, may not be the same as for the caller! TODO should the source be identified here? Ops can nest...
    rm_nm :: NameId
  }

{-
Client:
rn <- resolveName 0
hndl <- getHandle rn 1
bytes <- readFromNomiaHandle hndl 2

Server:
(req, resolveKey) <- readRequest
emitEvent resolveKey "this reduces to something else in my namespace"
sendResult req someRN
async $ do
  pause 10 hours
  doSomeStuff someRN
  emitEvent resolveKey "I did stuff"
(op, _handleKey) <- readReqFromReq req
sendResult hndl op
(op, readKey) <- readReqFromHandle hndl
emitEvent readKey "downloading the file behind the scenes"
sendResult bytes op
async $ do
  -- continue reading for a while
  emitEvent readKey "everything downloaded"
-}

-- TODO should this be monad generic?
-- TODO This should be full-fledged O11Y. DSL for emit-side filtering (e.g. event white/blacklist), maybe separate pipes in capability world, etc.
-- TODO Contravariant logging?
-- TODO separate paths for client vs component logs
-- TODO Async vs sync and op keys, see above comment block
-- TODO "ready to hand" event, maybe with an updated handle? native handle? anomic name?
-- Events are emitted best-effort, cannot assume every event will be conveyed
newtype Observer = Observer
  { observe :: ReductionMessage -> IO ()
  }

maybeObserve :: Maybe Observer -> ReductionMessage -> IO ()
maybeObserve Nothing _ = pure ()
maybeObserve (Just (Observer {..})) rm = observe rm

-- TODO resource types go here in index?
-- TODO Should be able to recover the original (plain data) name here, and context
-- TODO Update the observer? In capability world this would have to be passed along the socket maybe
class ResolvedName rn where
  -- TODO type magic to have namespace-specific monads instead of IO

  -- TODO Should we have an e param for errors, or shove it into a?
  -- TODO Should we have a d param for arguments? In that case it's not really an AnomicNameType, so much as an AnomicNameOp... If we need it!
  makeAnomic :: forall a. Typeable a => rn -> AnomicNameType a -> IO (Maybe a)

data SomeResolvedName where
  SomeResolvedName :: (ResolvedName a) => a -> SomeResolvedName

data Namespace = Namespace
  {resolveInNs :: NameId -> Maybe Observer -> IO (Maybe SomeResolvedName)}

newtype Resolver = Resolver (HashMap Text (Params -> Namespace))

-- TODO Differentiate "unknown namespace" from "failed to resolve in namespace"
resolveName :: Resolver -> Name -> Maybe Observer -> IO (Maybe SomeResolvedName)
resolveName (Resolver nsmap) (AtomicName (PrimitiveNamespace params nsid) nm) mObs =
  case Map.lookup nsid nsmap of
    Just ns -> resolveInNs (ns params) nm mObs
    Nothing -> pure Nothing
resolveName (Resolver _nsmap) (AtomicName (NameNamespace _nsnm) _nm) _mObs =
  $notImplemented -- Resolve to a handle, call resolveName there
